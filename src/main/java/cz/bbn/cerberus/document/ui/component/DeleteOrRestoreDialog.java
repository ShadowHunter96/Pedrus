package cz.bbn.cerberus.document.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.document.dto.DocumentDto;
import cz.bbn.cerberus.document.interfaces.DeleteOrRestoreAction;
import cz.bbn.cerberus.translation.Transl;

public class DeleteOrRestoreDialog extends AppDialog {

    private final AppInfiniteGrid<DocumentDto> grid;
    private final DeleteOrRestoreAction deleteOrRestoreAction;
    private final String name;

    public DeleteOrRestoreDialog(AppInfiniteGrid<DocumentDto> grid, DeleteOrRestoreAction deleteOrRestoreAction,
                                 String name) {
        this.grid = grid;
        this.deleteOrRestoreAction = deleteOrRestoreAction;
        this.name = name;
        initComponent();
    }

    public void initComponent() {
        setTitle(Transl.get("Delete or restore document"));
        setTextAsContent(Transl.get("Do you permanent delete or restore document ?"));

        Button delete = VaadinComponents.getButton(Transl.get("Permanent delete"));
        delete.setDisableOnClick(true);
        delete.addClickListener(buttonClickEvent -> {
            deleteOrRestoreAction.deleteOrRestoreItem(name, false);
            grid.loadData();
            delete.setEnabled(true);
            this.close();
        });

        Button unlink = VaadinComponents.getButton(Transl.get("Restore file"));
        unlink.setDisableOnClick(true);
        unlink.addClickListener(buttonClickEvent -> {
            deleteOrRestoreAction.deleteOrRestoreItem(name, true);
            grid.loadData();
            unlink.setEnabled(true);
            this.close();
        });

        Button close = VaadinComponents.getCloseButton();
        close.addClickListener(buttonClickEvent -> this.close());

        addButtons(delete, unlink, close);
    }
}
