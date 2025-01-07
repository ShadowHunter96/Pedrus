package cz.bbn.cerberus.ares;

import java.util.ArrayList;
import java.util.List;

public class EsAnswer {

    private final String text;

    private final List<EsItem> itemList = new ArrayList<>();

    public EsAnswer(String text) {
        this.text = text;
        parseItems();
    }

    public List<EsItem> getItemList() {
        return itemList;
    }

    private void parseItems() {
        if (text.contains("<dtt:S>") && text.contains("</dtt:S>")) {
            String[] textArray = text.split("</dtt:S>");
            for (String itemText : textArray) {
                if (itemText.contains("<dtt:ico>") && itemText.contains("</dtt:ico>")) {
                    EsItem esItem = new EsItem(itemText);
                    itemList.add(esItem);
                }
            }
        }
    }
}
