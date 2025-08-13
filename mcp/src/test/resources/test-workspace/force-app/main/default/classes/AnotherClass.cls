public class AnotherClass {
    public void useTestClass() {
        TestClass tc = new TestClass();
        tc.processData('test data');
        
        String message = TestClass.getMessage();
        System.debug(message);
    }
}