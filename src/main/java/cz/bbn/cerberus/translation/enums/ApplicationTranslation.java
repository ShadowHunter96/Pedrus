package cz.bbn.cerberus.translation.enums;

public enum ApplicationTranslation {

    CS("cs"), EN("en");

    private final String description;

    ApplicationTranslation(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
