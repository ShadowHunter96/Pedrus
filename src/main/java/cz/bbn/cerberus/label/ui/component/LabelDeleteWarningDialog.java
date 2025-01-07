package cz.bbn.cerberus.label.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.textfield.TextArea;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class LabelDeleteWarningDialog extends AppDialog {

    private final String id;
    private final List<String> list;
    private final DeleteAction deleteAction;
    private final AppInfiniteGrid grid;
    private final AppEnv appEnv;

    public LabelDeleteWarningDialog(String id, List<String> list, DeleteAction deleteAction,
                                    AppInfiniteGrid grid, AppEnv appEnv) {
        this.id = id;
        this.list = list;
        this.deleteAction = deleteAction;
        this.grid = grid;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        setTitle(Transl.get("Delete label"));

        H4 label = new H4(Transl.get("The label is attached to subjects. " +
                "Are you sure you want to delete label {0} ?", id));
        setContent(label);

        TextArea textArea = new TextArea(Transl.get("Subjects"));
        textArea.setReadOnly(true);
        textArea.setValue(StringUtils.join(list, ", "));
        textArea.setWidthFull();
        textArea.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
        setContent(textArea);

        Button delete = VaadinComponents.getButton(Transl.get("Delete"), VaadinIcon.TRASH.create());
        delete.addClassName(RobotFrameworkVariables.CONFIRM_DIALOG_BUTTON_CLASS.getValue());
        delete.addClickListener(buttonClickEvent -> {
            deleteAction.deleteItem(id);
            grid.loadData();
            this.close();
            SuccessNotification.showDeleteSuccess(appEnv);
        });
        addButtons(delete);

        Button cancelButton = VaadinComponents.getCloseButton();
        cancelButton.addClickListener(buttonClickEvent ->
                this.close());

        addButtons(cancelButton);
    }
}
