package cz.bbn.cerberus.adis;

public enum AdisReliable {

    YES, NO, UNDEFINED;

    public static AdisReliable fromString(String name) {
        for (AdisReliable adisReliable : AdisReliable.values()) {
            if (adisReliable.name().equals(name)) {
                return adisReliable;
            }
        }
        return UNDEFINED;
    }

}
