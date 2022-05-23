public class Main {

    public static void main(String... args) {
        System.out.println("---- mutable ----");

        {
            var mutableFullName = new mutable.FullName("Sasaki", "Kojiro");
            var anotherMutableFullName = mutableFullName;
            anotherMutableFullName.setFamilyName("Sato");

            System.out.println(anotherMutableFullName);
            // Expect to print out "Sasaki"
            // but actually print "Sato" out
            System.out.println(mutableFullName);
        }

        System.out.println("---- immutable ----");

        {
            var immutableFullName = new immutable.FullName("Sasaki", "Kojiro");
            var anotherImmutableFullName = immutableFullName;
            // Create a new object when the method called
            System.out.println(anotherImmutableFullName.setFamilyName("Sato"));
            // Keep holding the former value
            System.out.println(anotherImmutableFullName);
            // Keep holding the former value
            System.out.println(immutableFullName);
        }
    }
}
