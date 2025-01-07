package cz.bbn.cerberus.commons.component.ui.dialog;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.textfield.TextArea;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class WarningListDialog extends AppDialog {

    private final List<String> list;
    private final List<String> secondList;
    private final List<String> thirdList;
    private final List<String> fourthList;

    private final String heading;
    private final String text;
    private final String listText;
    private final String secondListText;
    private final String thirdListText;
    private final String fourthListText;

    private DeleteAction deleteAction = null;
    private String id = null;

    public WarningListDialog(List<String> list, String heading, String text, String listText) {
        this.list = list;
        this.secondList = new ArrayList<>();
        this.thirdList = new ArrayList<>();
        this.fourthList = new ArrayList<>();
        this.heading = heading;
        this.text = text;
        this.listText = listText;
        this.secondListText = "";
        this.thirdListText = "";
        this.fourthListText = "";
        init();
    }

    public WarningListDialog(List<String> firstList, List<String> secondList, List<String> thirdList,
                             List<String> fourthList, String heading, String text, String firstListText,
                             String secondListText, String thirdListText, String fourthListText,
                             DeleteAction deleteAction, String id) {
        this.list = firstList;
        this.secondList = secondList;
        this.thirdList = thirdList;
        this.fourthList = fourthList;
        this.heading = heading;
        this.text = text;
        this.listText = firstListText;
        this.secondListText = secondListText;
        this.thirdListText = thirdListText;
        this.fourthListText = fourthListText;
        this.deleteAction = deleteAction;
        this.id = id;
        init();
    }

    public void init() {
        setTitle(heading);

        H4 label = new H4(text);
        setContent(label);
        if (!list.isEmpty()) {
            TextArea textArea = new TextArea(listText);
            textArea.setWidth(CssVariables.TEXT_AREA_OVERFLOW_WIDTH.getValue());
            textArea.setReadOnly(true);
            textArea.setValue(StringUtils.join(list, ", "));
            textArea.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
            setContent(textArea);
        }
        if (!secondList.isEmpty()) {
            TextArea secondTextArea = new TextArea(secondListText);
            secondTextArea.setWidth(CssVariables.TEXT_AREA_OVERFLOW_WIDTH.getValue());
            secondTextArea.setReadOnly(true);
            secondTextArea.setValue(StringUtils.join(secondList, ", "));
            secondTextArea.setHeight(!list.isEmpty() ?
                    CssVariables.OVERFLOW_LAYOUT_HEIGHT.getValue() : CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
            setContent(secondTextArea);
        }

        if (!thirdList.isEmpty()) {
            TextArea thirdTextArea = new TextArea(thirdListText);
            thirdTextArea.setWidth(CssVariables.TEXT_AREA_OVERFLOW_WIDTH.getValue());
            thirdTextArea.setReadOnly(true);
            thirdTextArea.setValue(StringUtils.join(thirdList, ", "));
            thirdTextArea.setHeight(!list.isEmpty() && !secondList.isEmpty() ?
                    CssVariables.OVERFLOW_LAYOUT_HEIGHT.getValue() : CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
            setContent(thirdTextArea);
        }

        if (!fourthList.isEmpty()) {
            TextArea fourthTextArea = new TextArea(fourthListText);
            fourthTextArea.setWidth(CssVariables.TEXT_AREA_OVERFLOW_WIDTH.getValue());
            fourthTextArea.setReadOnly(true);
            fourthTextArea.setValue(StringUtils.join(fourthList, ", "));
            fourthTextArea.setHeight(!list.isEmpty() && !secondList.isEmpty() && !thirdList.isEmpty() ?
                    CssVariables.OVERFLOW_LAYOUT_HEIGHT.getValue() : CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
            setContent(fourthTextArea);
        }

        Button cancelButton = VaadinComponents.getCloseButton();
        cancelButton.addClickListener(buttonClickEvent ->
                this.close());

        addButtons(cancelButton);

        if (deleteAction != null && id != null) {
            Button deleteAnyway = VaadinComponents.getDeleteButton();
            deleteAnyway.setText(Transl.get("Delete anyway"));
            deleteAnyway.addClickListener(e -> {
                deleteAction.deleteItem(id);
                this.close();
            });
            addButtons(deleteAnyway);
        }
    }
}
