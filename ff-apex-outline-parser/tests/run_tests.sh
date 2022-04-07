cp -r tests/apex-link/samples/fflib-apex-mocks/fflib-apex-mocks tests/apex-link/samples/fflib-apex-common

cp -r tests/apex-link/samples/fflib-apex-mocks/fflib-apex-mocks tests/apex-link/samples/fflib-apex-common-samplecode
cp -r tests/apex-link/samples/fflib-apex-common/fflib-apex-common tests/apex-link/samples/fflib-apex-common-samplecode

cp -r tests/apex-link/samples/fflib-apex-mocks/fflib-apex-mocks tests/apex-link/samples/at4dx
cp -r tests/apex-link/samples/fflib-apex-common/fflib-apex-common tests/apex-link/samples/at4dx
cp -r tests/apex-link/samples/force-di/force-di tests/apex-link/samples/at4dx

JAR_FILE=$(pwd)/./jvm/target/scala-2.13/parser-assembly*.jar
acc=0
for i in $(find tests/apex-link/samples -mindepth 2 -maxdepth 2 -type d | grep -v -e "einstein-ai/einstein-ai" -e "NPSP/NPSP" -e "EDA/EDA" -e "HEDAP/HEDAP" -e "Cumulus/Cumulus" -e "Volunteers-for-Salesforce/Volunteers-for-Salesforce")
do
    echo $i
    (cd $i ; java -jar ${JAR_FILE} -seq .)
    rv=$?
    echo $rv
    acc=$(($acc + $rv))
done
echo $acc
exit $acc
