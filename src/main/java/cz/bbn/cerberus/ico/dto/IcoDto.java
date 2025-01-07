package cz.bbn.cerberus.ico.dto;

import cz.bbn.cerberus.adis.AdisReliable;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class IcoDto {

    private String court;
    private String fileNumber;
    private String register;

    private String ico;
    private String companyName;
    private String lawForm;
    private String address;
    private String enlistDate;

    private String capital;

    private String companionNames;

    private AdisReliable reliable;
    private String unreliableFrom;
    private String standardAccount;
    private String nonStandardAccount;
    private String dic;
}
