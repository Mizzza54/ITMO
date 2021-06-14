package info.kgeorgiy.ja.gerasimov.implementor.test;

import java.io.*;
import java.nio.file.*;

/**
 * @author Michael Gerasimov
 * start: 11.02.2021
 * @version -
 */
public class TestClass {

    private final String ERROR_ZERO = "0000000000000000";
    private final int BUFFER_SIZE = 2048;

    private String hash(final String path) {
        try {
            return String.format("%016x", evaluationHash(path));
        } catch (Exception e) {
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
            return hash; // NOTE: bad idea catch NullPointerException -> FIX ✅
        } catch (final IllegalArgumentException e) {
            throw new WalkerExceptions("IllegalArgumentException -> evaluationHash -> in newInputStream invalid combination of options is specified", e);
        } catch (final UnsupportedOperationException e) {
            throw new WalkerExceptions("UnsupportedOperationException -> evaluationHash -> in newInputStream unsupported option is specified", e);
        } catch (final SecurityException e) {
            throw new WalkerExceptions("SecurityException -> evaluationHash -> newInputStream cant read", e);
        } catch (final IOException e) {
            throw new WalkerExceptions("IOException -> evaluationHash", e);
        }
    }

    private void run(final String inputFile, final String outputFile) throws WalkerExceptions {
        // NOTE: it's not guaranteed they will be closed when declared like that -> FIX by autocloseable ✅
        Path scannerPath;
        Path writerPath;

        try {
            // NOTE: redundant encoding UTF-8 -> FIX ✅
            scannerPath = Paths.get(inputFile);
        } catch (InvalidPathException e) {
            throw new WalkerExceptions("InvalidPathException -> run -> wrong inputFile", e);
        }

        try {
            Path Parent = Paths.get(outputFile).getParent();
            if (Parent != null) {
                Files.createDirectories(Parent);
            }
            writerPath = Paths.get(outputFile);
        } catch (FileAlreadyExistsException e) {
            throw new WalkerExceptions("FileAlreadyExistsException -> run -> createDirectories cant create new dir because it is Already Exists", e);
        } catch (InvalidPathException e) {
            throw new WalkerExceptions("InvalidPathException -> run -> wrong outputFile", e);
        } catch (SecurityException e) {
            throw new WalkerExceptions("SecurityException -> run -> cant create Dir", e);
        } catch (IOException e) {
            throw new WalkerExceptions("IOException -> run", e);
        }

        String line;

        try (BufferedReader scanner = Files.newBufferedReader(scannerPath)) {
            try (BufferedWriter writer = Files.newBufferedWriter(writerPath)) {
                while ((line = scanner.readLine()) != null) {
                    String hash = hash(line);
                    writer.write(hash + " " + line); // NOTE: not cross platform \n -> FIX ✅
                    writer.newLine();
                }
            } catch (SecurityException e) {
                throw new WalkerExceptions("SecurityException -> run -> writer cant write", e);
            } catch (IOException e) {
                throw new WalkerExceptions("IOException -> run -> Problems with writer", e);
            }
        } catch (SecurityException e) {
            throw new WalkerExceptions("SecurityException -> run -> scanner cant read", e);
        } catch (IOException e) {
            throw new WalkerExceptions("IOException -> run -> Problems with scanner", e);
        }

    }

    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Error: Wrong CLI arguments -> main");
            return;
        }

        TestClass Walk = new TestClass();
        try {
            Walk.run(args[0], args[1]);
        } catch (WalkerExceptions e) {
            System.err.println(e.getMessage());
        }
    }

    static class WalkerExceptions extends Exception {
        public WalkerExceptions(String message, Exception e) {
            super(e.getMessage() + "\nError: " + message);
        }

        public WalkerExceptions(String message) {
            super("\nError: " + message);
        }
    }
}
