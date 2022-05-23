package immutable;

import java.util.Objects;

public class FullName {
    private final String familyName;
    private final String firstName;

    public FullName(String familyName, String firstName) {
        this.familyName = familyName;
        this.firstName = firstName;
    }

    public String getFamilyName() {
        return familyName;
    }

    public String getFirstName() {
        return firstName;
    }

    public FullName setFamilyName(String familyName) {
        return new FullName(familyName, firstName);
    }

    public FullName setFirstName(String firstName) {
        return new FullName(familyName, firstName);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        FullName fullName = (FullName) o;
        return Objects.equals(familyName, fullName.familyName) && Objects.equals(firstName, fullName.firstName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(familyName, firstName);
    }

    @Override
    public String toString() {
        return "FullName{" +
                "familyName='" + familyName + '\'' +
                ", firstName='" + firstName + '\'' +
                '}';
    }
}
