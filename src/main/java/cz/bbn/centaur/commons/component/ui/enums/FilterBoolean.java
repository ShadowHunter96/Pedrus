package cz.bbn.cerberus.commons.component.ui.enums;

import cz.bbn.cerberus.translation.Transl;

public enum FilterBoolean {

    ALL("Show all", null), YES("Yes", true), NO("No", false);

    private final String title;
    private final Boolean value;

    FilterBoolean(String title, Boolean value) {
        this.title = title;
        this.value = value;
    }

    public static FilterBoolean getValueForFilter(String value) {
        if(value == null || value.equals("null")){
            return ALL;
        }
        Boolean actValue = Boolean.valueOf(value);
        if(actValue){
            return YES;
        }else{
            return NO;
        }
    }

    public static String getValueForGrid(Boolean value) {
        if(value == null || value.equals(Boolean.FALSE)){
            return Transl.get(NO.getTitle());
        }else{
            return Transl.get(YES.getTitle());
        }
    }

    public String getTitle() {
        return title;
    }

    public Boolean getValue() {
        return value;
    }
}
