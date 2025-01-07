package cz.bbn.cerberus.document.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.document.interfaces.DeleteOrUnlinkAction;
import cz.bbn.cerberus.translation.Transl;

public class DeleteOrUnlinkDialog extends AppDialog {

    private final DeleteOrUnlinkAction deleteOrUnlinkAction;
    private final String id;
    private final String entityId;
    private final AppInfiniteGrid<?> grid;

    public DeleteOrUnlinkDialog(DeleteOrUnlinkAction deleteOrUnlinkAction, String id, String entityId,
                                AppInfiniteGrid<?> grid) {
        this.deleteOrUnlinkAction = deleteOrUnlinkAction;
        this.id = id;
        this.entityId = entityId;
        this.grid = grid;
        initComponent();
    }

    public void initComponent() {
        setTitle(Transl.get("Delete or unlink document"));
        setTextAsContent(Transl.get("Do you delete or unlink document"));

        Button delete = VaadinComponents.getButton(Transl.get("Delete"));
        delete.setDisableOnClick(true);
        delete.addClickListener(buttonClickEvent -> {
            deleteOrUnlinkAction.deleteItem(id, entityId, true);
            grid.loadData();
            delete.setEnabled(true);
            this.close();
        });

        Button unlink = VaadinComponents.getButton(Transl.get("Unlink"));
        unlink.setDisableOnClick(true);
        unlink.addClickListener(buttonClickEvent -> {
            deleteOrUnlinkAction.deleteItem(id, entityId, false);
            grid.loadData();
            unlink.setEnabled(true);
            this.close();
        });

        Button close = VaadinComponents.getCloseButton();
        close.addClickListener(buttonClickEvent -> this.close());

        addButtons(delete, unlink, close);
    }
}
