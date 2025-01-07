package cz.bbn.cerberus.attendance.dto;


import java.awt.Color;

public enum AttendenceEnum {

    WEEKEND("Weekend", "", Color.GRAY, "grey"),
    PUBLIC_HOLIDAY("Public holiday", "S", new Color(52, 110, 235), "blue"),
    BUSINESS_TRIP("Business trip", "SC", Color.ORANGE, "orange"),
    BUSINESS_TRIP_NO_WORK_DAY("Business trip", "SC", Color.ORANGE, "orange"),
    ILL("Ill", "N", Color.CYAN, "cyan"),
    HOLIDAY("Holiday", "D", Color.YELLOW, "yellow"),
    HALF_HOLIDAY("Half holiday", "1/D", Color.YELLOW, "yellow"),
    HOME_OFFICE("Home office", "HO", new Color(37, 150, 190), "light-blue"),
    PAID_LEAVE("Paid leave", "PV", Color.GREEN, "green"),
    UNPAID_LEAVE("Unpaid leave", "NP", Color.PINK, "pink"),
    WORK_DAY("Work day", "1", Color.WHITE, "white"),
    UNFILLED_WORK_DAY("Unfilled workday", "1", Color.WHITE, "light-red"),
    HALF_WORK_DAY("Half workday", "1", Color.WHITE, "light-red"),
    UNEMPLOYED("Unemployed", "X", Color.GRAY, "grey");

    private final String description;
    private final String value;
    private final String color;
    private final Color pdfColor;

    AttendenceEnum(String description, String value, Color pdfColor, String color) {
        this.description = description;
        this.value = value;
        this.pdfColor = pdfColor;
        this.color = color;
    }

    public String getDescription() {
        return description;
    }

    public String getValue() {
        return value;
    }

    public String getColor() {
        return color;
    }

    public Color getPdfColor() {
        return pdfColor;
    }

}
