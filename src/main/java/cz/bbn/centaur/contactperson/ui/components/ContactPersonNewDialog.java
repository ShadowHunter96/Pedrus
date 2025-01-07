package cz.bbn.cerberus.contactperson.ui.components;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contactperson.ContactPersonComponentOperation;
import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.contactperson.dto.TypeByObject;
import cz.bbn.cerberus.contactperson.ui.components.tab.ContactPersonDetailTab;
import cz.bbn.cerberus.phoneprefix.PhonePrefixService;
import cz.bbn.cerberus.phoneprefix.dto.PhonePrefixDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class ContactPersonNewDialog extends AppDialog {

    private final AppInfiniteGrid<?> grid;
    private final AppEnv appEnv;
    private final ContactPersonComponentOperation contactPersonComponentOperation;
    private final PhonePrefixService phonePrefixService;
    private final List<TypeByObject> typeByObjectList;

    public ContactPersonNewDialog(AppInfiniteGrid<?> grid, AppEnv appEnv,
                                  ContactPersonComponentOperation contactPersonComponentOperation,
                                  PhonePrefixService phonePrefixService,
                                  List<TypeByObject> typeByObjectList) {
        this.grid = grid;
        this.appEnv = appEnv;
        this.contactPersonComponentOperation = contactPersonComponentOperation;
        this.phonePrefixService = phonePrefixService;
        this.typeByObjectList = typeByObjectList;
        init();
    }

    private void init() {
        setTitle(Transl.get("Add contact person"));

        ContactPersonDto dto = new ContactPersonDto();
        dto.setPhonePrefixDto(new PhonePrefixDto("CZ", "+420"));
        dto.setPhonePrefix2Dto(new PhonePrefixDto("CZ", "+420"));
        dto.setTypeByObjectList(typeByObjectList);

        ContactPersonDetailTab contactPersonDetailTab = new ContactPersonDetailTab(dto,
                contactPersonComponentOperation.getSaveAction(this),
                contactPersonComponentOperation.getContactPersonTypeDtoList(), appEnv,
                phonePrefixService.findAllPhonePrefixDtoList(), true, false);
        setContent(contactPersonDetailTab);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            contactPersonDetailTab.saveItem();
            if (grid != null) {
                grid.loadData();
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }
}
