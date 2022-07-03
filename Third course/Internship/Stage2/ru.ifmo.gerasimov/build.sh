out_path="./script_out/"
osgi_core_path="../lib/osgi.core-8.0.0.jar"
module_name="ru.ifmo.gerasimov"
module_path="./ru/ifmo/gerasimov"
sources_paths="${out_path}/sources.txt"
classes_paths="classes.txt"

mkdir -p $out_path
find "./src/main/java/${module_path}" -name "*.java" > $sources_paths
javac -cp $osgi_core_path -d $out_path @$sources_paths
cd $out_path || exit
find $module_path -name "*.class" > $classes_paths
jar cvfm "${module_name}.jar" ../src/main/resources/META-INF/MANIFEST.MF @$classes_paths
rm $classes_paths
cd .. || exit
rm $sources_paths