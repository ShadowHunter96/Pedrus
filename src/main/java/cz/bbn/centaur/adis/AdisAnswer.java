package cz.bbn.cerberus.adis;

import cz.mfcr.adis.rozhranicrpdph.InformaceOPlatciType;
import cz.mfcr.adis.rozhranicrpdph.StatusNespolehlivyPlatceResponse;

import java.util.HashMap;
import java.util.Map;

public class AdisAnswer {

    private final Map<String, AdisItem> itemMap = new HashMap<>();

    public AdisAnswer(StatusNespolehlivyPlatceResponse response) {
        for (InformaceOPlatciType information : response.getStatusPlatceDPH()) {
            itemMap.put(information.getDic(), new AdisItem(information));
        }
    }

    public Map<String, AdisItem> getItemMap() {
        return itemMap;
    }
}
