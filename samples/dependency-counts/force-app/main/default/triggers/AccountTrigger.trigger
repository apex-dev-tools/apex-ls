trigger AccountTrigger on Account(
	before insert
) {
    new TransDep().func();
}
