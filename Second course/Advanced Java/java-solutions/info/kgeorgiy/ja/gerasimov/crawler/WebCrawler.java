package info.kgeorgiy.ja.gerasimov.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;

/**
 * @author Michael Gerasimov
 * start: 02.05.2021
 * @version -
 */
public class WebCrawler implements Crawler {
    private enum Default {
        DEFAULT_DEPTH(1),
        DEFAULT_DOWNLOADERS(10),
        DEFAULT_EXTRACTORS(10),
        DEFAULT_PER_HOST(10),
        NONE(0);

        private final int value;

        Default(final int value) {
            this.value = value;
        }

        public int getValue() {
            return this.value;
        }

        public static Default castIndexToDefault(final int index) {
            return values()[index];
        }
    }

    private final Downloader downloader;
    private final ExecutorService downloaders;
    private final ExecutorService extractors;
    private final Map<String, Semaphore> hostMap;
    private final int perHost;

    public WebCrawler(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
        this.downloader = downloader;
        this.downloaders = Executors.newFixedThreadPool(downloaders);
        this.extractors = Executors.newFixedThreadPool(extractors);
        this.perHost = perHost;
        this.hostMap = new ConcurrentHashMap<>();
    }



    @Override
    public Result download(final String url, final int depth) {
        final Set<String> downloaded = ConcurrentHashMap.newKeySet();
        final Map<String, IOException> errors = new ConcurrentHashMap<>();
        final Phaser phaser = new Phaser(1);
        methodBFS(new Node(url, 1), depth, downloaded, errors, phaser);
        phaser.arriveAndAwaitAdvance();
        downloaded.removeAll(errors.keySet());
        return new Result(List.copyOf(downloaded), errors);
    }

    @Override
    public void close() {
        shutdownAndAwaitTermination(downloaders);
        shutdownAndAwaitTermination(extractors);
    }

    // :NOTE: Не дождались
    private static void shutdownAndAwaitTermination(final ExecutorService pool) {
        pool.shutdown();
        try {
            if (!pool.awaitTermination(60, TimeUnit.SECONDS)) {
                pool.shutdownNow();
                if (!pool.awaitTermination(60, TimeUnit.SECONDS)) {
                    System.err.println("Pool did not terminate");
                }
            }
        } catch (final InterruptedException ie) {
            pool.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    private void methodBFS(final Node start,
                     final int depth,
                     final Set<String> downloaded,
                     final Map<String, IOException> errors,
                     final Phaser phaser) {
        final Queue<Node> queue = new ConcurrentLinkedQueue<>();
        queue.add(start);
        downloaded.add(start.getUrl());
        while (!queue.isEmpty()) {
            final Node v = queue.poll();
            try {
                final String host = URLUtils.getHost(v.getUrl());
                hostMap.putIfAbsent(host, new Semaphore(perHost));
                // :NOTE: Не парный
                phaser.register();
                downloaders.submit(createDownloadTask(host, queue, v, depth, downloaded, phaser, errors));
            } catch (final MalformedURLException e) {
                errors.put(v.getUrl(), e);
            }

            if (queue.isEmpty()) {
                phaser.arriveAndAwaitAdvance();
            }
        }
    }

    private Runnable createDownloadTask(final String host,
                                        final Queue<Node> queue,
                                        final Node current,
                                        final int depth,
                                        final Set<String> visited,
                                        final Phaser phaser,
                                        final Map<String, IOException> errors) {
        return () -> {
            try {
                // :NOTE: Простои
                hostMap.get(host).acquire();
                final Document document = downloader.download(current.getUrl());
                if (current.getDepth() < depth) {
                    phaser.register();
                    extractors.submit(createExtractorTask(document, queue,
                            current.getDepth(), visited, phaser));
                }
            } catch (final IOException e) {
                errors.put(current.getUrl(), e);
            } catch (final InterruptedException e) {
                e.printStackTrace();
            } finally {
                hostMap.get(host).release();
                phaser.arriveAndDeregister();
            }
        };
    }

    public static Runnable createExtractorTask(final Document document,
                                               final Queue<Node> queue,
                                               final int currentDepth,
                                               final Set<String> visited,
                                               final Phaser phaser) {
        return () -> {
            try {
                for (final String url : document.extractLinks()) {
                    if (visited.add(url)) {
                        queue.add(new Node(url, currentDepth + 1));
                    }
                }
            } catch (final IOException e) {
                // ignored
            } finally {
                phaser.arriveAndDeregister();
            }
        };
    }

    private static int getValue(final String[] args, final int index) {
        if (args.length > index && args[index] != null) {
            try {
                return Integer.parseInt(args[index]);
            } catch (final NumberFormatException e) {
                return Default.castIndexToDefault(index).getValue();
            }
        } else {
            return Default.castIndexToDefault(index).getValue();
        }
    }

    public static void main(final String[] args) throws IOException {
        if (args == null || args.length == 0 || args[0] == null) {
            System.err.println("Invalid args.\nUsage: WebCrawler url [depth [downloads [extractors [perHost]]]]");
            return;
        }

        final String url = args[0];
        final int depth = getValue(args, 1);
        final int downloaders = getValue(args, 2);
        final int extractors = getValue(args, 3);
        final int perHost = getValue(args, 4);

        try (final WebCrawler crawler = new WebCrawler(new CachingDownloader(), downloaders, extractors, perHost)) {
            final Result result = crawler.download(url, depth);

            System.out.println("Downloaded:");
            for (final String string : result.getDownloaded()) {
                System.out.println(string);
            }
            System.out.println("\n\nErrors:");
            for (final String string : result.getErrors().keySet()) {
                System.out.println(string);
            }
        } catch (final IOException e) {
            System.err.println("Can not create CachingDownloader()");
        }
    }
}