public class LibraryTestClass {
    public void publicMethod() {
        // This should NOT be flagged as unused in a library project
    }
    
    private void privateMethod() {
        // This SHOULD be flagged as unused even in a library project
    }
}