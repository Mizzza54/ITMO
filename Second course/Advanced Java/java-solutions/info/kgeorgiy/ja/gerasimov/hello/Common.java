package info.kgeorgiy.ja.gerasimov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;
import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * @author Michael Gerasimov
 * start: 23.05.2021
 * @version -
 */
public class Common {
    private final static int DEFAULT_AWAIT_TIMEOUT_MILLISECONDS = 1000;
    private final static Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;

    protected static byte[] stringToBytes(final String string) {
        return string.getBytes(DEFAULT_CHARSET);
    }

    protected static String bytesToString(final ByteBuffer byteBuffer) {
        return DEFAULT_CHARSET.decode(byteBuffer).toString();
    }

    protected static void putAndSend(final DatagramChannel datagramChannel,
                                     final ByteBuffer byteBuffer,
                                     final byte[] data,
                                     final SocketAddress socketAddress) throws IOException {
        byteBuffer.clear();
        byteBuffer.put(data);
        byteBuffer.flip();
        datagramChannel.send(byteBuffer, socketAddress);
    }

    protected static SocketAddress receive(final DatagramChannel datagramChannel,
                                           final ByteBuffer byteBuffer) throws IOException {
        byteBuffer.clear();
        final SocketAddress socketAddress = datagramChannel.receive(byteBuffer);
        byteBuffer.flip();
        return socketAddress;
    }

    protected static boolean isValidMessage(String message, int threadIndex, int requestIndex) {
        return message.matches("[\\D]*" + threadIndex + "[\\D]*" + requestIndex + "[\\D]*");
    }

    private static boolean isArgsNull(final String[] args) {
        for (String str : args) {
            if (str == null) {
                return true;
            }
        }
        return false;
    }

    protected static void mainClient(final String[] args, final HelloClient client) {
        if (args == null || args.length != 5 || isArgsNull(args)) {
            System.err.println("Invalid args");
            return;
        }

        try {
            final String host = args[0];
            final int port = Integer.parseInt(args[1]);
            final String prefix = args[2];
            final int threads = Integer.parseInt(args[3]);
            final int requests = Integer.parseInt(args[4]);
            client.run(host, port, prefix, threads, requests);
        } catch (NumberFormatException e) {
            System.err.println("Invalid number");
        }
    }

    protected static void mainServer(final String[] args, final HelloServer server) {
        if (args == null || args.length != 2 || isArgsNull(args)) {
            System.err.println("Invalid args");
            return;
        }

        try {
            final int port = Integer.parseInt(args[0]);
            final int threads = Integer.parseInt(args[1]);
            server.start(port, threads);
        } catch (NumberFormatException e) {
            System.err.println("Invalid number");
        }
    }

    protected static void shutdownAndAwaitTermination(final ExecutorService pool) {
        if (pool == null) {
            return;
        }
        pool.shutdown();
        try {
            if (!pool.awaitTermination(DEFAULT_AWAIT_TIMEOUT_MILLISECONDS, TimeUnit.MILLISECONDS)) {
                pool.shutdownNow();
                if (!pool.awaitTermination(DEFAULT_AWAIT_TIMEOUT_MILLISECONDS, TimeUnit.MILLISECONDS)) {
                    System.err.println("Pool did not terminate");
                }
            }
        } catch (InterruptedException ie) {
            pool.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }
}
