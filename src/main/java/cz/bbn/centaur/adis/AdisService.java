package cz.bbn.cerberus.adis;

import cz.mfcr.adis.rozhranicrpdph.RozhraniCRPDPH;
import cz.mfcr.adis.rozhranicrpdph.RozhraniCRPDPH_Service;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class AdisService {

    private AdisService() {
    }

    public AdisAnswer getDataFromAdisByDic(List<String> dicList) {
        RozhraniCRPDPH_Service serviceImpl = new RozhraniCRPDPH_Service();
        RozhraniCRPDPH service = serviceImpl.getRozhraniCRPDPHSOAP();
        RequestParam statusNespolehlivyPlatceRequest = new RequestParam(dicList);
        return new AdisAnswer(service.getStatusNespolehlivyPlatce(statusNespolehlivyPlatceRequest));
    }
}
