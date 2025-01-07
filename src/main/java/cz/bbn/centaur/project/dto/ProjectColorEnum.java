package cz.bbn.cerberus.project.dto;

import cz.bbn.cerberus.translation.Transl;

public enum ProjectColorEnum {

    PING("Ping", "#ffb3ba"),
    ORANGE("Orange", "#ffdfba"),
    YELLOW("Yellow", "#ffffba"),
    GREEN("Green", "#baffc9"),
    BLUE("Blue", "#bae1ff");


    private final String name;
    private final String code;

    ProjectColorEnum(String name, String code) {
        this.name = name;
        this.code = code;
    }

    public String getName() {
        return Transl.get(name);
    }

    public String getCode() {
        return code;
    }

    public static ProjectColorEnum getById(String code) {
        for (ProjectColorEnum colorEnum : ProjectColorEnum.values()) {
            if (colorEnum.code.equals(code)) {
                return colorEnum;
            }
        }
        return BLUE;
    }
}
