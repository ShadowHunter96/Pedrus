package cz.bbn.cerberus.adis;

import cz.mfcr.adis.rozhranicrpdph.StatusNespolehlivyPlatceRequest;

import java.util.List;

public class RequestParam extends StatusNespolehlivyPlatceRequest {

    public RequestParam(List<String> dicList) {
        this.dic = dicList;
    }
}