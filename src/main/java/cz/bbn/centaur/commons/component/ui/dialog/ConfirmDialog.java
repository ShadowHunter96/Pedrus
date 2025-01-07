package cz.bbn.cerberus.commons.component.ui.dialog;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.ConfirmAction;
import cz.bbn.cerberus.translation.Transl;

public class ConfirmDialog extends AppDialog {

    private final String confirmText;
    private final ConfirmAction confirmAction;

    public ConfirmDialog(String confirmText, ConfirmAction confirmAction) {
        this.confirmText = confirmText;
        this.confirmAction = confirmAction;
        init();
    }

    private void init() {
        setTitle(Transl.get("Confirmation"));
        setTextAsContent(confirmText);

        Button closeButton = VaadinComponents.getCloseButton();
        closeButton.addClickListener(buttonClickEvent -> close());

        Button submitButton = VaadinComponents.getConfirmButton();
        submitButton.setDisableOnClick(true);
        submitButton.addClickListener(event -> {
            confirmAction.confirm();
            submitButton.setEnabled(true);
            close();
        });

        addButtons(closeButton, submitButton);
    }

}
