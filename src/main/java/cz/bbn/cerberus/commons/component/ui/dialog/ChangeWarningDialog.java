package cz.bbn.cerberus.commons.component.ui.dialog;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.textfield.TextArea;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class ChangeWarningDialog extends AppDialog {

    private final List<String> list;
    private final ComboBox<?> comboBox;
    private final Button changeButton;

    private final String title;
    private final String text;
    private final String labelText;

    public ChangeWarningDialog(List<String> list, ComboBox<?> comboBox, Button changeButton,
                               String title, String text, String labelText) {
        this.list = list;
        this.comboBox = comboBox;
        this.changeButton = changeButton;
        this.title = title;
        this.text = text;
        this.labelText = labelText;
        init();
    }

    public void init() {
        setTitle(Transl.get(title));
        Div div = new Div();
        H4 label = new H4(Transl.get(text));
        TextArea textArea = new TextArea(Transl.get(labelText));
        textArea.setReadOnly(true);
        textArea.setValue(StringUtils.join(list, ", "));
        textArea.setWidthFull();
        textArea.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
        div.add(label);
        div.add(textArea);
        setContent(div);

        div.add(comboBox);

        Button cancelButton = VaadinComponents.getCloseButton();
        cancelButton.addClickListener(buttonClickEvent -> this.close());

        addButtons(cancelButton, changeButton);
    }
}
