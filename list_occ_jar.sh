#~/bin/sh

jar tf lib/occjava.jar | grep class | sed 's/\//./g' | sed 's/\.class//' | while read class; do
  echo -n //
  javap -classpath lib/occjava.jar -public $class
  echo
  echo
done
