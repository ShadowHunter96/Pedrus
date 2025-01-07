package cz.bbn.cerberus.commons.component.ui.dialog;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.translation.Transl;


public class BackButtonDialog extends AppDialog {

    public BackButtonDialog() {
        init();
    }

    void init() {
        setTitle(Transl.get("Back"));
        setTextAsContent(Transl.get("Are you sure you want to leave? Changes will not be saved"));

        Button yesButton = VaadinComponents.getButton(Transl.get("Yes"));
        yesButton.addClassName(RobotFrameworkVariables.YES_BUTTON_CLASS.getValue());
        yesButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        yesButton.addClickListener(buttonClickEvent -> {
                    UI.getCurrent().getPage().getHistory().back();
                    this.close();
                }
        );

        Button noButton = VaadinComponents.getButton(Transl.get("No"));
        noButton.addClassName(RobotFrameworkVariables.NO_BUTTON_CLASS.getValue());
        noButton.addClickListener(buttonClickEvent -> this.close());

        this.addButtons(noButton, yesButton);
    }
}
