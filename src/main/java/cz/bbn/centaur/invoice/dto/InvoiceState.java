package cz.bbn.cerberus.invoice.dto;

import cz.bbn.cerberus.translation.Transl;

public enum InvoiceState {

    IMPORTED("Imported"),
    NEW("New"),
    TO_BE_INVOICED("To be invoiced"),
    INVOICED("Invoiced"),
    PAYED("Payed"),
    CANCELED("Canceled");

    private final String value;

    InvoiceState(String value) {
        this.value = value;
    }

    public String getTranslatedName() {
        return Transl.get(value);
    }

    public static InvoiceState getState(String stateString) {
        for (InvoiceState state : InvoiceState.values()) {
            if (state.name().equals(stateString)) {
                return state;
            }
        }
        return NEW;
    }
}
