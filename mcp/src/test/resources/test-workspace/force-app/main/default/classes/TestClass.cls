public class TestClass {
    public static String getMessage() {
        return 'Hello from TestClass';
    }
    
    public void processData(String input) {
        System.debug('Processing: ' + input);
        String result = getMessage();
        System.debug('Result: ' + result);
    }
    
    public class InnerClass {
        public String innerField;
        
        public InnerClass() {
            this.innerField = 'inner value';
        }
    }
}