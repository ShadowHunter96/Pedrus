package cz.bbn.cerberus.ares;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class VrAnswer {

    private final String text;

    private String basicInformation = "";

    private String capital = "";
    private String enlistDate = "";
    private String address = "";
    private String lawForm = "";
    private String register = "";
    private String fileNumber = "";
    private String court = "";
    private String ico = "";
    private String companyName = "";
    private String companionNames = "";

    public VrAnswer(String text) {
        this.text = text;
        setBasicInformation();
        parseCapital();
        parseEnlistDate();
        parseAddress();
        parseLawForm();
        parseRegister();
        parseFileNumber();
        parseCourt();
        parseIco();
        parseCompanyName();
        parseCompanionNames();
    }

    public String getCapital() {
        return capital;
    }

    public String getEnlistDate() {
        return enlistDate;
    }

    public String getAddress() {
        return address;
    }

    public String getLawForm() {
        return lawForm;
    }

    public String getRegister() {
        return register;
    }

    public String getFileNumber() {
        return fileNumber;
    }

    public String getCourt() {
        return court;
    }

    public String getIco() {
        return ico;
    }

    public String getCompanyName() {
        return companyName;
    }

    public String getCompanionNames() {
        return companionNames;
    }

    private void setBasicInformation() {
        if (text.contains("<are:ZakladniUdaje>") && text.contains("</are:ZakladniUdaje>")) {
            basicInformation =
                    text.substring(text.indexOf("<are:ZakladniUdaje>") + 19, text.indexOf("</are:ZakladniUdaje>"));
        }
    }

    private void parseCapital() {
        if (text.contains("<are:Kapital>") && text.contains("</are:Kapital>")) {
            String capStr = text.substring(text.indexOf("<are:Kapital>") + 13, text.indexOf("</are:Kapital>"));
            if (capStr.contains("<are:value>") && capStr.contains("</are:value>")) {
                String valueStr = capStr.substring(capStr.indexOf("<are:value>") + 11, capStr.indexOf("</are:value>"));
                String[] valueArr = valueStr.split(";");
                if (valueArr.length == 2) {
                    String value = StringUtils.join(valueArr, ",");
                    if (capStr.contains("<are:typ>") && capStr.contains("</are:typ>")) {
                        String type = capStr.substring(capStr.indexOf("<are:typ>") + 9, capStr.indexOf("</are:typ>"));
                        if ("KORUNY".equals(type)) {
                            value = value + " Kč";
                        }
                        if ("EURA".equals(type)) {
                            value = value + " €";
                        }
                    }
                    this.capital = value;
                }
            }
        }
    }

    private void parseEnlistDate() {
        if (basicInformation.contains("<are:DatumZapisu>") && basicInformation.contains("</are:DatumZapisu>")) {
            String enlStr = basicInformation.substring(basicInformation.indexOf("<are:DatumZapisu>") + 17,
                    basicInformation.indexOf("</are:DatumZapisu>"));
            String[] enlArr = enlStr.split("-");
            if (enlArr.length == 3) {
                enlistDate = enlArr[2] + "." + enlArr[1] + "." + enlArr[0];
            }
        }
    }

    private void parseAddress() {
        if (basicInformation.contains("<are:Sidlo>") && basicInformation.contains("</are:Sidlo>")) {
            String addStr = basicInformation.substring(basicInformation.indexOf("<are:Sidlo>") + 11,
                    basicInformation.indexOf("</are:Sidlo>"));
            StringBuilder addTemp = new StringBuilder();
            String street = "";
            String strNmb = "";
            String strNmb2 = "";
            String psc = "";
            String city = "";
            String cityPart = "";
            String country = "";
            if (addStr.contains("<dtt:NazevUvp>") && addStr.contains("</dtt:NazevUvp>")) {
                street = addStr.substring(addStr.indexOf("<dtt:NazevUvp>") + 14, addStr.indexOf("</dtt:NazevUvp>"));
            }
            if (addStr.contains("<dtt:CisloDomu>") && addStr.contains("</dtt:CisloDomu>")) {
                strNmb = addStr.substring(addStr.indexOf("<dtt:CisloDomu>") + 15, addStr.indexOf("</dtt:CisloDomu>"));
            }
            if (addStr.contains("<dtt:CisloOr>") && addStr.contains("</dtt:CisloOr>")) {
                strNmb2 = addStr.substring(addStr.indexOf("<dtt:CisloOr>") + 13, addStr.indexOf("</dtt:CisloOr>"));
            }
            if (addStr.contains("<dtt:Psc>") && addStr.contains("</dtt:Psc>")) {
                psc = addStr.substring(addStr.indexOf("<dtt:Psc>") + 9, addStr.indexOf("</dtt:Psc>"));
            }
            if (addStr.contains("<dtt:NazevObce>") && addStr.contains("</dtt:NazevObce>")) {
                city = addStr.substring(addStr.indexOf("<dtt:NazevObce>") + 15, addStr.indexOf("</dtt:NazevObce>"));
            }
            if (addStr.contains("<dtt:NazevCastob>") && addStr.contains("</dtt:NazevCastob>")) {
                cityPart = addStr.substring(addStr.indexOf("<dtt:NazevCastob>") + 17,
                        addStr.indexOf("</dtt:NazevCastob>"));
            }
            if (addStr.contains("<dtt:NazevStatu>") && addStr.contains("</dtt:NazevStatu>")) {
                country = addStr.substring(addStr.indexOf("<dtt:NazevStatu>") + 16,
                        addStr.indexOf("</dtt:NazevStatu>"));
            }

            if (!"".equals(street.strip())) {
                addTemp.append(street);
            }
            addTemp.append(" ");
            if (!"".equals(strNmb.strip())) {
                addTemp.append(strNmb);
            }
            if (!"".equals(strNmb2.strip())) {
                addTemp.append("/").append(strNmb2);
            }
            addTemp.append(",");
            if (!"".equals(psc.strip())) {
                addTemp.append(" ").append(psc);
            }
            if (!"".equals(city.strip())) {
                addTemp.append(" ").append(city);
            }
            if (!"".equals(cityPart.strip())) {
                addTemp.append(" - ").append(cityPart);
            }
            if (!"".equals(country.strip())) {
                addTemp.append(", ").append(country);
            }

            this.address = addTemp.toString();
        }
    }

    private void parseLawForm() {
        if (basicInformation.contains("<are:PravniForma>") && basicInformation.contains("</are:PravniForma>")) {
            String lawStr = basicInformation.substring(basicInformation.indexOf("<are:PravniForma>") + 17,
                    basicInformation.indexOf("</are:PravniForma>"));
            StringBuilder lawFormTemp = new StringBuilder();
            String lawCode = "";
            String lawName = "";
            if (lawStr.contains("<are:KodPravniForma>") && lawStr.contains("</are:KodPravniForma>")) {
                lawCode = lawStr.substring(lawStr.indexOf("<are:KodPravniForma>") + 20,
                        lawStr.indexOf("</are:KodPravniForma>"));
            }
            if (lawStr.contains("<are:NazevPravniForma>") && lawStr.contains("</are:NazevPravniForma>")) {
                lawName = lawStr.substring(lawStr.indexOf("<are:NazevPravniForma>") + 22,
                        lawStr.indexOf("</are:NazevPravniForma>"));
            }
            if (!"".equals(lawCode.strip())) {
                lawFormTemp.append(lawCode);
            }
            if (!"".equals(lawCode.strip()) && !"".equals(lawName.strip())) {
                lawFormTemp.append(" - ");
            }
            if (!"".equals(lawName.strip())) {
                lawFormTemp.append(lawName);
            }
            this.lawForm = lawFormTemp.toString();
        }
    }

    private void parseRegister() {
        if (basicInformation.contains("<are:Rejstrik>") && basicInformation.contains("</are:Rejstrik>")) {
            this.register = basicInformation.substring(basicInformation.indexOf("<are:Rejstrik>") + 14,
                    basicInformation.indexOf("</are:Rejstrik>"));
        }
    }

    private void parseFileNumber() {
        if (basicInformation.contains("<are:OddilVlozka>") && basicInformation.contains("</are:OddilVlozka>")) {
            this.fileNumber = basicInformation.substring(basicInformation.indexOf("<are:OddilVlozka>") + 17,
                    basicInformation.indexOf("</are:OddilVlozka>"));
        }
    }

    private void parseCourt() {
        if (basicInformation.contains("<are:SpisovaZnacka>") && basicInformation.contains("</are:SpisovaZnacka>")) {
            String courtStr = basicInformation.substring(basicInformation.indexOf("<are:SpisovaZnacka>") + 19,
                    basicInformation.indexOf("</are:SpisovaZnacka>"));
            if (courtStr.contains("<dtt:Nazev>") && courtStr.contains("</dtt:Nazev>")) {
                court = courtStr.substring(courtStr.indexOf("<dtt:Nazev>") + 11, courtStr.indexOf("</dtt:Nazev>"));
            }
        }
    }

    private void parseIco() {
        if (basicInformation.contains("<are:Ico>") && basicInformation.contains("</are:Ico>")) {
            String icoStr = basicInformation.substring(basicInformation.indexOf("<are:Ico>") + 9,
                    basicInformation.indexOf("</are:Ico>"));
            if (icoStr.contains("<are:value>") && icoStr.contains("</are:value>")) {
                ico = icoStr.substring(icoStr.indexOf("<are:value>") + 11, icoStr.indexOf("</are:value>"));
            }
        }
    }

    private void parseCompanyName() {
        if (basicInformation.contains("<are:ObchodniFirma>") && basicInformation.contains("</are:ObchodniFirma>")) {
            String nameStr = basicInformation.substring(basicInformation.indexOf("<are:ObchodniFirma>") + 19,
                    basicInformation.indexOf("</are:ObchodniFirma>"));
            if (nameStr.contains("<are:value>") && nameStr.contains("</are:value>")) {
                companyName = nameStr.substring(nameStr.indexOf("<are:value>") + 11, nameStr.indexOf("</are:value>"));
            }
        }
    }

    private void parseCompanionNames() {
        if (text.contains("<are:StatutarniOrgan>") && text.contains("</are:StatutarniOrgan>")) {
            List<String> compList = new ArrayList<>();
            String compStr = text.substring(text.indexOf("<are:StatutarniOrgan>") + 21,
                    text.indexOf("</are:StatutarniOrgan>"));
            if (compStr.contains("</are:Clen>")) {
                String[] compArr = compStr.split("</are:Clen>");
                for (String comp : compArr) {
                    addCompToArr(comp, compList);
                }
            }
            companionNames = StringUtils.join(compList, ", ");
        }
    }

    private void addCompToArr(String comp, List<String> compList) {
        StringBuilder compName = new StringBuilder();
        if (comp.contains("<are:titulPred>") && comp.contains("</are:titulPred>")) {
            compName.append(comp, comp.indexOf("<are:titulPred>") + 15,
                    comp.indexOf("</are:titulPred>")).append(" ");
        }
        if (comp.contains("<are:jmeno>") && comp.contains("</are:jmeno>")) {
            compName.append(comp, comp.indexOf("<are:jmeno>") + 11, comp.indexOf("</are:jmeno>"));
        }
        if (comp.contains("<are:prijmeni>") && comp.contains("</are:prijmeni>")) {
            compName.append(" ").append(comp, comp.indexOf("<are:prijmeni>") + 14,
                    comp.indexOf("</are:prijmeni>"));
        }
        compList.add(compName.toString());
    }
}
