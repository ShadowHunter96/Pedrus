package cz.bbn.cerberus.invoice.dto;

import cz.bbn.cerberus.translation.Transl;

public enum InvoicingPeriod {

    MONTHLY("Monthly"),
    YEARLY("Yearly"),
    HALF_YEARLY("Half yearly"),
    QUARTERLY("Quarterly");

    private final String text;

    InvoicingPeriod(String text) {
        this.text = text;
    }

    public static InvoicingPeriod getFromString(String periodString) {
        for (InvoicingPeriod period : InvoicingPeriod.values()) {
            if (period.toString().equals(periodString)) {
                return period;
            }
        }
        return YEARLY;
    }

    public String getText() {
        return Transl.get(text);
    }
}
