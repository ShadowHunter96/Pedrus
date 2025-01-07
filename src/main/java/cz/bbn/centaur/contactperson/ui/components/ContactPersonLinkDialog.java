package cz.bbn.cerberus.contactperson.ui.components;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.Set;

public class ContactPersonLinkDialog extends AppDialog {

    private final ContactPersonGridComponent grid;

    public ContactPersonLinkDialog(ItemsAction<ContactPersonDto> itemsAction,
                                   ContactPersonGetMultipleEvent linkContactEvent,
                                   ContactPersonFilterComponent filter, AppEnv appEnv) {
        this.setTitle(Transl.get("Link contact person"));
        grid = new ContactPersonGridComponent(itemsAction, appEnv);
        Button linkItemsButton = VaadinComponents.getLinkMultipleButton();
        linkItemsButton.setDisableOnClick(true);
        linkItemsButton.addClickListener(linkContactEvent.get(this, linkItemsButton));
        this.setContent(filter, grid);
        this.addButtons(linkItemsButton);
        this.addCloseButton();
        grid.loadData();
    }

    public void loadData() {
        grid.loadData();
    }

    public Set<ContactPersonDto> getSelected() {
        return grid.getSelectedItems();
    }
}
