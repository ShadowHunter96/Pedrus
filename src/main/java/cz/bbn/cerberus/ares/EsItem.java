package cz.bbn.cerberus.ares;

public class EsItem {

    private final String text;

    private String ico = "";
    private String dic = "";
    private String name = "";
    private String address = "";

    public EsItem(String text) {
        this.text = text;
        parseIco();
        parseDic();
        parseName();
        parseAddress();
    }

    public String getIco() {
        return ico;
    }

    public String getDic() {
        return dic;
    }

    public String getName() {
        return name;
    }

    public String getAddress() {
        return address;
    }

    private void parseIco() {
        if (text.contains("<dtt:ico>") && text.contains("</dtt:ico>")) {
            ico = text.substring(text.indexOf("<dtt:ico>") + 9, text.indexOf("</dtt:ico>"));
        }
    }

    private void parseDic() {
        if (text.contains("<dtt:p_dph>dic=") && text.contains("</dtt:p_dph>")) {
            dic = text.substring(text.indexOf("<dtt:p_dph>dic=") + 15, text.indexOf("</dtt:p_dph>"));
        }
    }

    private void parseName() {
        if (text.contains("<dtt:ojm>") && text.contains("</dtt:ojm>")) {
            name = text.substring(text.indexOf("<dtt:ojm>") + 9, text.indexOf("</dtt:ojm>"));
        }
    }

    private void parseAddress() {
        if (text.contains("<dtt:jmn>") && text.contains("</dtt:jmn>")) {
            address = text.substring(text.indexOf("<dtt:jmn>") + 9, text.indexOf("</dtt:jmn>"));
        }
    }
}