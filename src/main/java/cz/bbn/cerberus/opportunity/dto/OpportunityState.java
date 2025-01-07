package cz.bbn.cerberus.opportunity.dto;

import java.util.ArrayList;
import java.util.List;

public enum OpportunityState {

    IMPORTED(true),
    NOT_SUBMITED(true),
    IN_PROGRESS(true),
    SUBMITTED(true),
    WON(true),
    NOT_WON(true),
    OBSOLETE(false),
    CANCELED(true);

    private final boolean active;

    OpportunityState(boolean active) {
        this.active = active;
    }

    public static List<OpportunityState> getAll() {
        List<OpportunityState> stateList = new ArrayList<>();
        for (OpportunityState state : OpportunityState.values()) {
            if (state.active) {
                stateList.add(state);
            }
        }
        return stateList;
    }

    public static boolean stateExist(String value) {
        for (OpportunityState state : OpportunityState.getAll()) {
            if (state.name().equals(value)) {
                return true;
            }
        }
        return false;
    }
}
