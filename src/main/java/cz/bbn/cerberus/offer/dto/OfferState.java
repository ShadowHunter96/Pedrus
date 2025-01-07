package cz.bbn.cerberus.offer.dto;

public enum OfferState {

    IMPORTED, IN_PROGRESS, SUBMITTED, WON, NOT_WON, CANCELED;

    public static boolean stateExist(String stateStr) {
        for (OfferState offerState : OfferState.values()) {
            if (offerState.name().equals(stateStr)) {
                return true;
            }
        }
        return false;
    }
}
