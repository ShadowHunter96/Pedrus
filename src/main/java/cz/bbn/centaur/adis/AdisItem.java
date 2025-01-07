package cz.bbn.cerberus.adis;

import cz.mfcr.adis.rozhranicrpdph.InformaceOPlatciType;
import cz.mfcr.adis.rozhranicrpdph.NespolehlivyPlatceType;
import cz.mfcr.adis.rozhranicrpdph.ZverejnenyUcetType;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class AdisItem {

    private final InformaceOPlatciType response;

    private AdisReliable reliable = AdisReliable.UNDEFINED;
    private String standardAccount = "";
    private String nonStandardAccount = "";
    private String unreliableFrom = "";

    public AdisItem(InformaceOPlatciType response) {
        this.response = response;
        parseReliable();
        parseAccount();
        parseUnreliableFrom();
    }

    public AdisReliable getReliable() {
        return reliable;
    }

    public String getStandardAccount() {
        return standardAccount;
    }

    public String getNonStandardAccount() {
        return nonStandardAccount;
    }

    public String getUnreliableFrom() {
        return unreliableFrom;
    }

    private void parseReliable() {
        if (NespolehlivyPlatceType.ANO == response.getNespolehlivyPlatce()) {
            reliable = AdisReliable.NO;
        }
        if (NespolehlivyPlatceType.NE == response.getNespolehlivyPlatce()) {
            reliable = AdisReliable.YES;
        }
    }

    private void parseAccount() {
        List<String> standardAccountList = new ArrayList<>();
        List<String> unstandardAccountList = new ArrayList<>();
        if (response.getZverejneneUcty() != null && response.getZverejneneUcty().getUcet() != null) {
            for (ZverejnenyUcetType account : response.getZverejneneUcty().getUcet()) {
                addAccountToList(account, unstandardAccountList, standardAccountList);
            }
        }
        standardAccount = StringUtils.join(standardAccountList, ", ");
        nonStandardAccount = StringUtils.join(unstandardAccountList, ", ");
    }

    private void addAccountToList(ZverejnenyUcetType account,
                                  List<String> unstandardAccountList,
                                  List<String> standardAccountList) {
        StringBuilder tempAccount = new StringBuilder();
        if (account.getNestandardniUcet() != null && account.getNestandardniUcet().getCislo() != null) {
            tempAccount.append(account.getNestandardniUcet().getCislo());
            unstandardAccountList.add(tempAccount.toString());
        }

        if (account.getStandardniUcet() != null && account.getStandardniUcet().getCislo() != null) {
            if (account.getStandardniUcet().getPredcisli() != null) {
                tempAccount.append(account.getStandardniUcet().getPredcisli()).append(" ");
            }
            tempAccount.append(account.getStandardniUcet().getCislo());
            if (account.getStandardniUcet().getKodBanky() != null) {
                tempAccount.append("/").append(account.getStandardniUcet().getKodBanky());
            }
            standardAccountList.add(tempAccount.toString());
        }
    }

    private void parseUnreliableFrom() {
        if (response.getDatumZverejneniNespolehlivosti() != null) {
            unreliableFrom = response.getDatumZverejneniNespolehlivosti().getDay() + "."
                    + response.getDatumZverejneniNespolehlivosti().getMonth() + "."
                    + response.getDatumZverejneniNespolehlivosti().getYear();
        }
    }
}
