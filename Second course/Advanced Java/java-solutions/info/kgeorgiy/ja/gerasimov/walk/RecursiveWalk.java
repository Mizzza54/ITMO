package info.kgeorgiy.ja.gerasimov.walk;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

/**
 * @author Michael Gerasimov
 * start: 11.02.2021
 * @version -
 */
public class RecursiveWalk {

    private final String ERROR_ZERO = "0000000000000000";
    private final int BUFFER_SIZE = 2048;

    private String hash(final String path) {
        try {
            return String.format("%016x", evaluationHash(path));
        } catch (WalkerExceptions e) {
            System.err.println(e.getMessage());
            return ERROR_ZERO;
        }
    }

    private long evaluationHash(final String path) throws WalkerExceptions {
        try {
            InputStream in = new BufferedInputStream(Files.newInputStream(Paths.get(path)));
            byte[] bytes = new byte[BUFFER_SIZE];
            int size = in.read(bytes);
            long hash = 0;
            while (size != -1) {
                for (int i = 0; i < size; i++) {
                    hash = (hash << 8) + (bytes[i] & 0xff);
                    long high = hash & 0xff00_0000_0000_0000L;
                    if (high != 0) {
                        hash ^= high >> 48;
                        hash &= ~high;
                    }
                }
                size = in.read(bytes);
            }
            return hash;
        } catch (final NullPointerException e) {
            throw new WalkerExceptions("NullPointerException -> evaluationHash", e);
        } catch (final IllegalArgumentException e) {
            throw new WalkerExceptions("IllegalArgumentException -> evaluationHash", e);
        } catch (final UnsupportedOperationException e) {
            throw new WalkerExceptions("UnsupportedOperationException -> evaluationHash", e);
        } catch (final SecurityException e) {
            throw new WalkerExceptions("SecurityException -> evaluationHash", e);
        } catch (final IOException e) {
            throw new WalkerExceptions("IOException -> evaluationHash", e);
        }
    }

    private void run(final String args1, final String args2) throws WalkerExceptions, IOException {
        BufferedReader scanner;
        BufferedWriter writer;

        try {
            scanner = Files.newBufferedReader(Paths.get(args1), StandardCharsets.UTF_8);
        } catch (InvalidPathException e) {
            throw new WalkerExceptions("InvalidPathException -> run", e);
        } catch (SecurityException e) {
            throw new WalkerExceptions("SecurityException -> run", e);
        } catch (IOException e) {
            throw new WalkerExceptions("IOException -> run", e);
        }

        try {
            writer = Files.newBufferedWriter(Paths.get(args2), StandardCharsets.UTF_8);
        } catch (InvalidPathException e) {
            throw new WalkerExceptions("InvalidPathException -> run", e);
        } catch (IllegalArgumentException e) {
            throw new WalkerExceptions("IllegalArgumentException -> run", e);
        } catch (UnsupportedOperationException e) {
            throw new WalkerExceptions("UnsupportedOperationException -> run", e);
        } catch (SecurityException e) {
            throw new WalkerExceptions("SecurityException -> run", e);
        } catch (IOException e) {
            throw new WalkerExceptions("IOException -> run", e);
        }

        String line;

        try {
            while ((line = scanner.readLine()) != null) {
                try {
                Path start = Paths.get(line);
                    Files.walkFileTree(start, new SimpleFileVisitor<>() {
                        @Override
                        public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException, InvalidPathException {
                            String hash = hash(file.toString());
                            System.out.println(file.toString());
                            writer.write(hash + " " + file.toString() + "\n");
                            return FileVisitResult.CONTINUE;
                        }

                        @Override
                        public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException, InvalidPathException {
                            writer.write(ERROR_ZERO + " " + file.toString() + "\n");
                            return FileVisitResult.CONTINUE;
                        }
                    });
                } catch (InvalidPathException e) {
                    writer.write(ERROR_ZERO + " " + line + "\n");
                    throw new WalkerExceptions("InvalidPathException -> walkFileTree", e);
                }
            }
            writer.close();
            scanner.close();
        } catch (SecurityException e) {
            throw new WalkerExceptions("SecurityException -> run", e);
        } catch (InvalidPathException e) {
            throw new WalkerExceptions("InvalidPathException -> run", e);
        } catch (IOException e) {
            throw new WalkerExceptions("IOException -> run", e);
        } catch (WalkerExceptions e) {
            writer.close();
            scanner.close();
            throw e;
        }
    }

    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Error: Wrong CLI arguments -> main");
            //System.exit(0);
            return;
        }


        RecursiveWalk RecursiveWalk = new RecursiveWalk();
        try {
            RecursiveWalk.run(args[0], args[1]);
        } catch (WalkerExceptions | IOException e) {
            System.err.println(e.getMessage());
        }
    }
}
