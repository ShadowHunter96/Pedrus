package cz.bbn.cerberus.commons.component.ui.dialog;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.translation.Transl;


public class DeleteConfirmDialog extends AppDialog {

    private final AppInfiniteGrid<?> solutiaGrid;
    private final String appId;
    private final AppEnv appEnv;
    private final boolean showDelete;
    private final String deleteQuestion;
    private final String title;

    public DeleteConfirmDialog(AppInfiniteGrid<?> solutiaGrid, String appId,
                               String deleteQuestion, AppEnv appEnv, boolean showDelete) {
        this.solutiaGrid = solutiaGrid;
        this.appId = appId;
        this.deleteQuestion = deleteQuestion;
        this.appEnv = appEnv;
        this.showDelete = showDelete;
        this.title = Transl.get("Delete");
        init();
    }

    public DeleteConfirmDialog(AppInfiniteGrid<?> solutiaGrid, String appId,
                               String deleteQuestion, AppEnv appEnv, String title) {
        this.solutiaGrid = solutiaGrid;
        this.appId = appId;
        this.deleteQuestion = deleteQuestion;
        this.appEnv = appEnv;
        this.showDelete = true;
        this.title = title;
        init();
    }

    private void init() {
        setTitle(title);
        setTextAsContent(deleteQuestion);

        Button delete = VaadinComponents.getButton(title, VaadinIcon.TRASH.create());
        delete.getElement().setProperty(TextValues.TITLE, title);
        delete.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        delete.setDisableOnClick(true);
        delete.addClassName(RobotFrameworkVariables.CONFIRM_DIALOG_BUTTON_CLASS.getValue());
        delete.addClickListener(this::onComponentEvent);

        Button closeButton = VaadinComponents.getCloseButton();
        closeButton.addClickListener(buttonClickEvent -> close());

        addButtons(closeButton);
        if (showDelete) {
            addButtons(delete);
        }
    }

    private void onComponentEvent(ClickEvent<Button> buttonClickEvent) {
        try {
            solutiaGrid.deleteItem(appId);
        } catch (SystemException ex) {
            ErrorNotification.show(ex, appEnv);
        }
        close();
    }
}
