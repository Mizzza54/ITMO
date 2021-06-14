package info.kgeorgiy.ja.gerasimov.implementor;


import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.function.Function;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;

/**
 * The Implementor class. This class generate implementations of classes or interfaces.
 * Command-line argument: The full name of the class / interface to generate the implementation.
 * As a result, the java code of the class with the "Impl" suffix be generated, extending (implementing) the specified class (interface).
 * <ul>
 * <li>Generated class classes name be same as classes name of the type token with Impl suffix added.</li>
 * <li>The generated class compile without errors.</li>
 * <li>The generated class not abstract.</li>
 * <li>Methods of the generated class ignore their arguments and return default values.</li>
 * </ul>
 *
 *
 * @author Michael Gerasimov
 * <p>start: 19.03.2021
 * @version 1.0
 */
public class Implementor implements JarImpler {
    /**
     * Path to home module
     */
    private static final Path PATH_TO_GERASIMOV = Paths.get("/Users/michael/Desktop/java-advanced-2021/modules/info.kgeorgiy.ja.gerasimov");

    /**
     * Entry point of the program. Runs the {@link #implement(Class, Path)} or {@link #implementJar(Class, Path)} method,
     * depending on the arguments of the command line
     * <p>
     * Usage: Implementor [-jar] Class name Target path.
     *
     * @param args arguments of CLI.
     */
    public static void main(final String[] args) {
        try {
            isValidCLIArgs(args);
            final Implementor implementor = new Implementor();
            try {
                final String className = args.length == 2 ? args[0] : args[1];
                final Path root = Paths.get(args.length == 2 ? args[1] : args[2]);
                final URLClassLoader loader = getClassLoader(PATH_TO_GERASIMOV);
                //Thread.currentThread().setContextClassLoader(loader);
                final Class<?> token = loader.loadClass(className);
                if (args.length == 2) {
                    implementor.implement(token, root);
                } else if (args[0].equals("-jar")) {
                    implementor.implementJar(token, root);
                } else {
                    throw new ImplerException("Incorrect CLI arguments");
                }
            } catch (final ClassNotFoundException e) {
                throw new ImplerException("Cant found this class");
            }
        } catch (final ImplerException e) {
            System.err.println(e.getMessage());
        }
    }

    /**
     * Simple initialization of {@link URLClassLoader}
     * @param root path where find classes
     * @return new URLClassLoader
     */
    public static URLClassLoader getClassLoader(final Path root) {
        try {
            return new URLClassLoader(new URL[]{root.toUri().toURL()});
        } catch (final MalformedURLException e) {
            throw new AssertionError(e);
        }
    }

    /**
     * Check validity of CLI arguments in {@link #main(String[])}.
     *
     * @param args arguments of CLI.
     * @throws ImplerException if invalid args
     */
    private static void isValidCLIArgs(final String[] args) throws ImplerException {
        if (args == null) {
            throw new ImplerException("args is null");
        }
        switch (args.length) {
            case 2:
                if (args[0] == null || args[1] == null) {
                    throw new ImplerException("args[0] or args[1] is null");
                }
                break;
            case 3:
                if (args[0] == null || args[1] == null || args[2] == null) {
                    throw new ImplerException("args[0], args[1] or args[2] is null");
                }
                if (!args[0].equals("-jar")) {
                    throw new ImplerException("Expected -jar");
                }
                break;
            default:
                throw new ImplerException("Expected 2 or 3 args");
        }
    }

    /**
     * Checks whether it is possible to implement class.
     *
     * @param token class token.
     * @return {@code true} if possible implement this class, otherwise false.
     */
    private static boolean isValidImplementorClass(final Class<?> token) {
        // :NOTE: Упростить
        return !token.isPrimitive()
                && token != Enum.class
                && !token.isArray()
                && !Modifier.isFinal(token.getModifiers())
                && !Modifier.isPrivate(token.getModifiers());
    }

    /**
     * Get path to new generated file.
     *
     * @param token     {@link Class} token to create a path for.
     * @param path      {@link Path} to root file.
     * @param extension extension of file.
     * @return {@link Path} to generated implementation of file
     */
    private static Path getPath(final Class<?> token, final Path path, final String extension) {
        // :NOTE: Работа с путями -> FIX by resolve
        return path.resolve(token.getPackageName().replace('.', File.separatorChar))
                .resolve(String.format("%sImpl.%s", token.getSimpleName(), extension));
    }

    /**
     * Create missing directories in path.
     *
     * @param path {@link Path} to file or directory.
     * @throws ImplerException if can't create directories.
     */
    private void createDirectories(final Path path) throws ImplerException {
        try {
            Files.createDirectories(path.getParent());
            Files.createFile(path);
        } catch (final IOException e) {
            throw new ImplerException("Can't create directories");
        }
    }

    /**
     * Create head of implements class.
     * Head contains:
     * <ul>
     *     <li>Package name</li>
     *     <li>Modifier of class</li>
     *     <li>Name of class</li>
     *     <li>Key words extends/implements</li>
     *     <li>Class(interfaces)  which extended(implemented)</li>
     * </ul>
     * @param token class token.
     * @return head of class in string.
     */
    private static String writeHeader(final Class<?> token) {
        return String.format(
                "%s %n public class %sImpl %s %s {%n",
                !token.getPackageName().isEmpty() ? "package " + token.getPackageName() + ";" : "",
                token.getSimpleName(),
                token.isInterface() ? "implements" : "extends",
                token.getCanonicalName()
        );
    }


    /**
     * Create body of implements class.
     * Body contains:
     * <ul>
     *     <li>Constructors</li>
     *     <li>Methods</li>
     * </ul>
     * @param token class token.
     * @return body of class in string.
     * @throws ImplerException if {@link #implementMethod(Class, Executable, boolean)} throw {@link ImplerException}.
     */
    private String writeBody(final Class<?> token) throws ImplerException {
        final StringBuilder stringBuilder = new StringBuilder();
        if (!token.isInterface()) {
            for (final Constructor<?> constructor : token.getDeclaredConstructors()) {
                // :NOTE: ??
                if (Modifier.isFinal(constructor.getModifiers())) {
                    continue;
                }
                stringBuilder.append(implementMethod(token, constructor, true));
            }
        }

        final Set<CustomMethod> allPublicMethods = getMethods(token);
        // :NOTE: stream
        for (final Method method : allPublicMethods.stream().map(CustomMethod::getMethod).collect(Collectors.toSet())) {
            if (Modifier.isFinal(method.getModifiers())
                    || !Modifier.isAbstract(method.getModifiers())
                    || Modifier.isPrivate(method.getModifiers())) {
                continue;
            }
            stringBuilder.append(implementMethod(token, method, false));
        }
        stringBuilder.append("}");
        return stringBuilder.toString();
    }

    /**
     * Implement method or constructor.
     * @param token token class.
     * @param executable is method or constructor of class.
     * @param isConstructor boolean values. True if executable - is constructor.
     * @return string implementation of executable.
     * @throws ImplerException if {@link #writeModifier(int)} throw {@link ImplerException}.
     */
    private String implementMethod(final Class<?> token, final Executable executable, final boolean isConstructor) throws ImplerException {
        return String.format("%s %s %s %s %s%n",
                writeModifier(executable.getModifiers()),
                // :NOTE: Передать части
                writeReturnType(!isConstructor ? ((Method) executable).getReturnType() : null),
                writeNameAndParameters(isConstructor ? token.getSimpleName() : executable.getName(),
                        executable.getParameters(),
                        isConstructor),
                writeThrowsExceptionAndTail(executable.getExceptionTypes()),
                writeImplementationAndTail(executable, isConstructor));
    }

    /**
     * Return all the methods that need to be implemented.
     *
     * @param token class token.
     * @return all the methods that need to be implemented.
     */
    private HashSet<CustomMethod> getMethods(Class<?> token) {
        // :NOTE: HashSet
        final HashSet<CustomMethod> result = new HashSet<>(getCustomMethods(token.getMethods()));
        while (token != null) {
            result.addAll(getCustomMethods(token.getDeclaredMethods()));
            token = token.getSuperclass();
        }
        return result;
    }

    /**
     * Convert {@link Method} to {@link CustomMethod}.
     * @param methods is array of method.
     * @return new List of {@link CustomMethod}.
     */
    private List<CustomMethod> getCustomMethods(final Method[] methods) {
        return List.of(methods).stream().map(CustomMethod::new).collect(Collectors.toList());
    }

    /**
     * A wrapper of {@link Method} for correct comparing.
     */
    private static class CustomMethod {
        /**
         * Instance of the method.
         */
        private final Method method;

        /**
         * Constructor of {@code CustomMethod}.
         *
         * @param method Instance of the method.
         */
        CustomMethod(final Method method) {
            this.method = method;
        }

        /**
         * Simple getter of field {@link #method}
         * @return field {@link #method}
         */
        public Method getMethod() {
            return method;
        }

        /**
         * Custom {@link Object#equals(Object)} method to equal two instances of {@link Method}.
         * Methods are equal if:
         * <ul>
         *     <li>The other method is not {@code null}</li>
         *     <li>Methods have same {@code hashCode}</li>
         * </ul>
         *
         * @param obj instance of the method
         * @return {@code true} if instances are equal
         */
        @Override
        public boolean equals(final Object obj) {
            if (obj == null) {
                return false;
            }
            // :NOTE: Коллизии
            if (obj instanceof CustomMethod) {
                return obj.hashCode() == hashCode();
            }
            return false;
        }

        /**
         * Calculate hash code of the {@link #method}
         *
         * @return hash code of the {@link #method}
         */
        @Override
        public int hashCode() {
            return method.getName().hashCode() + Arrays.hashCode(method.getParameterTypes());
        }
    }

    /**
     * Defining a modifier of method/constructor.
     * @param modifier numeric representation modifier of method/constructor.
     * @return {@code public} if method/constructor is not private.
     * @throws ImplerException if implementation method/constructor be private.
     * @see Executable#getModifiers()
     */
    private static String writeModifier(final int modifier) throws ImplerException {
        if (!Modifier.isPrivate(modifier)) {
            return "public";
        } else {
            throw new ImplerException("Can't implement private methods");
        }
    }

    /**
     * Defining a return type of method.
     * @param returnType return type of method.
     * @return type of method or empty string if returnType == null.
     */
    private String writeReturnType(final Class<?> returnType) {
        if (returnType == null) {
            return "";
        }
        if (returnType.isPrimitive()) {
            return returnType.getName();
        } else {
            return returnType.getCanonicalName();
        }
    }

    /**
     * Defining name and parameters of method/constructor.
     * @param name name of method/constructor.
     * @param parameters parameters of method/constructor.
     * @param isConstructor boolean value. True if executable - is constructor, otherwise false.
     * @return string representation of name and parameters of method/constructor
     */
    private String writeNameAndParameters(final String name, final Parameter[] parameters, final boolean isConstructor) {
        return String.format("%s%s(%s)", name,
                isConstructor ? "Impl" : "", getValues(parameters, s -> s.getType().getCanonicalName() + " " + s.getName()));
    }

    /**
     * Auxiliary function for excluding copy paste. Separated elements by {@code ","}.
     * @param values array of element.
     * @param function which should be applied.
     * @param <T> Type of array elements.
     * @return new string, where elements separated by {@code ","}.
     */
    private<T> String getValues(final T[] values, final Function<T, String> function) {
        return Arrays.stream(values)
                .map(function)
                .collect(Collectors.joining(","));
    }

    /**
     * Defining throws exceptions.
     * @param exceptions array of exceptions, which method/constructor can throw.
     * @return string representation of throws exceptions
     */
    private String writeThrowsExceptionAndTail(final Class<?>[] exceptions) {
        // :NOTE: Упростить
        return String.format("%s { %n", exceptions.length != 0
                ? String.format("throws %s", getValues(exceptions, Class::getCanonicalName))
                : "");
    }

    /**
     * Defining default value of method or defining constructor.
     * @param executable is method or constructor
     * @param isConstructor boolean values, true if executable is constructor.
     * @return implementation of method/constructor
     */
    private String writeImplementationAndTail(final Executable executable, final boolean isConstructor) {
        String string;
        if (!isConstructor) {
            final Method method = (Method) executable;
            final Class<?> returnType = method.getReturnType();
            if (method.getReturnType() != void.class) {
                final String defaultValue = returnType.isPrimitive() ? returnType == boolean.class ? "false" : "0" : "null";
                string = String.format("return %s;", defaultValue);
            } else {
                string = "";
            }
        } else {
            string = String.format("super(%s);", getValues(executable.getParameters(), Parameter::getName));
        }
        return String.format("%s %n }", string);
    }

    /**
     * Generate implementation target class.
     *
     * @param token type token to create implementation for.
     * @param root  root directory.
     * @throws ImplerException if can't implement target class
     */
    @Override
    public void implement(final Class<?> token, final Path root) throws ImplerException {
        if (isValidImplementorClass(token)) {
            final Path filePath = getPath(token, root, "java");
            createDirectories(filePath);
            try (final BufferedWriter writer = Files.newBufferedWriter(filePath)) {
                writer.write(toUnicode(writeHeader(token)));
                writer.write(toUnicode(writeBody(token)));
            } catch (final IOException e) {
                throw new ImplerException("Cant write implementation of this class");
            }
        } else {
            throw new ImplerException("This class can not be implemented");
        }
    }

    /**
     * Create Unicode representation of string.
     * @param string input string.
     * @return Unicode representation of string.
     */
    private String toUnicode(final String string) {
        final StringBuilder builder = new StringBuilder();
        for (final char c : string.toCharArray()) {
            if (c < 128) {
                builder.append(c);
            } else {
                builder.append(String.format("\\u%04X", (int) c));
            }
        }
        return builder.toString();
    }

    /**
     * Create temp directories for java files.
     * @param path input path.
     * @return new path to temp directories.
     * @throws ImplerException if cant create temp directory.
     */
    private Path createTempDirectory(final Path path) throws ImplerException {
        try {
            return Files.createTempDirectory(path.getParent(), "temp");
        } catch (final IOException e) {
            throw new ImplerException("Cant create temp directory");
        }
    }

    /**
     * Return class path for class.
     * @param token token class.
     * @return string representation class path for class.
     */
    private String getClassPath(final Class<?> token) {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new AssertionError(e);
        }
    }

    /**
     * Compile input file
     * @param token token class which will be compile.
     * @param path path where located file
     * @throws ImplerException if cant compile java file
     */
    private void compileFile(final Class<?> token, final Path path) throws ImplerException {
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        final String classPath = path + File.pathSeparator + getClassPath(token);
        final String[] args = new String[]{"-cp", classPath, getPath(token, path, "java").toString()};
        final int exitCode = compiler.run(null, null, null, args);
        if (exitCode != 0) {
            throw new ImplerException("Can't compile file");
        }
    }

    /**
     * {@link SimpleFileVisitor} for delete temp directories.
     */
    private static final SimpleFileVisitor<Path> DELETE_VISITOR = new SimpleFileVisitor<>() {

        /**
         * {@link SimpleFileVisitor} method.
         * @throws IOException if can't delete dir
         * @see SimpleFileVisitor
         */
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        /**
         * {@link SimpleFileVisitor} method.
         * @throws IOException if can't delete dir
         * @see SimpleFileVisitor
         */
        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    };

    /**
     * Function for delete temp directories.
     * @param path to tempDir.
     * @throws IOException if cant delete temp directories
     */
    private void clean(final Path path) throws IOException {
        if (Files.exists(path)) {
            Files.walkFileTree(path, DELETE_VISITOR);
        }
    }

    /**
     * Generates implementation of class and build a {@code .jar}
     * file which contains that implementation class.
     * @param token type token to create implementation for.
     * @param root where will be located {@code .jar} file.
     * @throws ImplerException if cant delete temp directories or Can't write JAR file.
     */
    @Override
    public void implementJar(final Class<?> token, final Path root) throws ImplerException {
        createDirectories(root);
        final Path tempPath = createTempDirectory(root.toAbsolutePath());
        implement(token, tempPath);
        compileFile(token, tempPath);
        final Manifest manifest = new Manifest();
        final Attributes attributes = manifest.getMainAttributes();
        attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");
        attributes.put(Attributes.Name.IMPLEMENTATION_VENDOR,"Michael");

        try {
            try (final JarOutputStream writerJar = new JarOutputStream(Files.newOutputStream(root), manifest)) {
                final String name = String.format("%s/%sImpl.class", token.getPackageName().replace('.', '/'), token.getSimpleName());
                writerJar.putNextEntry(new ZipEntry(name));
                Files.copy(tempPath.resolve(name), writerJar);
            } catch (final IOException e) {
                // :NOTE: ??
                e.printStackTrace();
                throw new ImplerException("Can't write JAR file", e);
            } finally {
                clean(tempPath);
            }
        } catch (final IOException e) {
            throw new ImplerException("Can't clean", e);
        }
    }
}
