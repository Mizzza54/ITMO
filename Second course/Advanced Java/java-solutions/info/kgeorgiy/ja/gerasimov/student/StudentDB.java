package info.kgeorgiy.ja.gerasimov.student;

import info.kgeorgiy.java.advanced.student.*;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Michael Gerasimov
 * start: 13.03.2021
 * @version -
 */
public class StudentDB implements StudentQuery {

    // :NOTE: why not static? and not in capitals -> FIX
    private static final Comparator<Student> BY_NAME_COMPARATOR = Comparator
            .comparing(Student::getLastName)
            .thenComparing(Student::getFirstName).reversed()
            .thenComparing(Student::getId);

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return studentsMapFunction(students, Student::getFirstName, Collectors.toList());
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return studentsMapFunction(students, Student::getLastName, Collectors.toList());
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return studentsMapFunction(students, Student::getGroup, Collectors.toCollection(ArrayList::new));
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return studentsMapFunction(students, s -> s.getFirstName() + " " + s.getLastName(), Collectors.toList());
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return students.stream()
                .map(Student::getFirstName)
                .collect(Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream()
                .max(Student::compareTo)
                .map(Student::getFirstName)
                .orElse("");
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortStudentsBySomething(students, Student::compareTo);
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortStudentsBySomething(students, BY_NAME_COMPARATOR);
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findStudentBySomething(students, name, Student::getFirstName);
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findStudentBySomething(students, name, Student::getLastName);
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return findStudentBySomething(students, group, Student::getGroup);
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        // :NOTE: you close stream, collect and open the stream again. it's ineffective -> FIX
        return students.stream()
                .filter(s -> Objects.equals(s.getGroup(), group))
                .collect(Collectors.toMap(
                        Student::getLastName,
                        Student::getFirstName,
                        BinaryOperator.minBy(String::compareTo)));
    }

    private <R extends Collection<T>, E, T> R studentsMapFunction(List<E> students, Function<E, T> func, Collector<T, ?, R> collector) {
        return students.stream()
                .map(func)
                .collect(collector);
    }

    // :NOTE: just make it a generic, instead of calling toString on objects -> FIX
    private <T> List<Student> findStudentBySomething(Collection<Student> students, T key, Function<Student, T> func) {
        return students.stream()
                .filter(s -> Objects.equals(func.apply(s), key))
                .sorted(BY_NAME_COMPARATOR)
                .collect(Collectors.toList());
    }


    private List<Student> sortStudentsBySomething(Collection<Student> students, Comparator<Student> comparator) {
        return students.stream()
                .sorted(comparator)
                .collect(Collectors.toList());
    }
}