package cz.bbn.cerberus.commons;

/**
 * Trida pro volani css stylu
 */
public enum CssVariables {

    LUMO_ERROR_COLOR("var(--lumo-error-text-color)"),
    LUMO_SUCCESS_COLOR("var(--lumo-success-text-color)"),
    LUMO_PRIMARY_TEXT_COLOR("var(--lumo-primary-text-color)"),
    LUMO_SECONDARY_TEXT_COLOR("var(--lumo-secondary-text-color)"),
    LUMO_PRIMARY_CONTRAST_COLOR("--lumo-primary-contrast-color"),
    LUMO_PRIMARY_COLOR("var(--lumo-primary-color)"),
    CURSOR_POINTER("cursor-pointer"),
    MENU_HEADER_MARGIN("menu-header-margin"),
    MARGIN_TOP("margin-top"),

    LUMO_SPACE_XS("var(--lumo-space-xs)"),
    LUMO_SPACE_S("var(--lumo-space-s)"),
    LUMO_SPACE_M("var(--lumo-space-m)"),
    LUMO_SPACE_L("var(--lumo-space-l)"),
    LUMO_SPACE_XL("var(--lumo-space-xl)"),

    FILTER_BUTTON_CLASS("filter-button"),
    DEFAULT_FIELD_WIDTH("15em"),
    MEDIUM_FIELD_WIDTH("30em"),
    DEFAULT_FIELD_HEIGHT("12.5em"),
    DEFAULT_TEXT_AREA_HEIGHT("12.5em"),
    SMALL_TEXT_AREA_HEIGHT("6.25em"),
    DEFAULT_TEXT_AREA_WIDTH("40em"),
    SMALL_TEXT_AREA_WIDTH("20em"),

    COMBOBOX_LARGE_WIDTH("20em"),

    OVERFLOW_LAYOUT_HEIGHT("calc(100% - 12.2em)"),

    TEXT_AREA_OVERFLOW_WIDTH("calc(100% - 4px)"),
    SLIDE_IN_CLASS("slide-animation-on"),
    SLIDE_OUT_CLASS("slide-animation-off");

    private final String value;

    CssVariables(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}

