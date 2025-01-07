package cz.bbn.cerberus.commons.enums;

public enum PctValues {

    PCT_0(0, "0 %"), PCT_25(25, "25 %"), PCT_50(50, "50 %"), PCT_75(75, "75 %"), PCT_100(100, "100 %");

    private final Integer pct;
    private final String value;

    PctValues(Integer pct, String value) {
        this.pct = pct;
        this.value = value;
    }

    public Integer getPct() {
        return pct;
    }

    public String getValue() {
        return value;
    }

    public static boolean exists(String name) {
        for (PctValues pct : PctValues.values()) {
            if (pct.name().equals(name)) {
                return true;
            }
        }
        return false;
    }
}
