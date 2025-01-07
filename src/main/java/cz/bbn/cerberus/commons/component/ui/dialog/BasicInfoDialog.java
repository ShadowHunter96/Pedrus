package cz.bbn.cerberus.commons.component.ui.dialog;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;

public class BasicInfoDialog extends AppDialog {

    private final String content;
    private final Component component;
    private final String title;

    public BasicInfoDialog(String content, String title) {
        this.content = content;
        this.title = title;
        this.component = null;
        init();
    }

    public BasicInfoDialog(Component component, String title) {
        this.component = component;
        this.title = title;
        this.content = null;
        init();
    }

    void init() {
        setTitle(title);

        if (content != null) {
            setTextAsContent(content);
        } else {
            setContent(component);
        }

        Button okButton = VaadinComponents.getCloseButton();
        okButton.addClickListener(buttonClickEvent -> this.close());

        this.addButtons(okButton);
    }
}
