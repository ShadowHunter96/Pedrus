package cz.bbn.cerberus.contactpersontype.ui.components;

import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.ContactPersonTypeTabComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.contactpersontype.ui.components.tab.ContactPersonTypeDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class ContactPersonTypeDetailComponent extends AppDetailCardComponent<ContactPersonTypeDto>
        implements AppBinderOperation<ContactPersonTypeDto> {

    public ContactPersonTypeDetailComponent(ContactPersonTypeDto dto, SaveAction<ContactPersonTypeDto> saveAction,
                                            boolean showSubmitButton, AppEnv appEnv,
                                            EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = getDto().getId() == null ? Transl.get("New contact person type") :
                Transl.get("Contact person type")
                        .concat(" - ")
                        .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE + "/" + ContactPersonTypeTabComponent.TAB_INDEX);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.CONTACT_PERSON_TYPE_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        this.add(new ContactPersonTypeDetailTabComponent(this, false));
    }
}
