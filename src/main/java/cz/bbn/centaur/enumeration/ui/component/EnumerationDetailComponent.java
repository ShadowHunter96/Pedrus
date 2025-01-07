package cz.bbn.cerberus.enumeration.ui.component;

import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.translation.Transl;

public class EnumerationDetailComponent
        extends AppDetailCardComponent<EnumerationDto> implements AppBinderOperation<EnumerationDto> {

    private final int tabIndex;
    private final EnumerationComponentOperation enumerationComponentOperation;

    public EnumerationDetailComponent(EnumerationDto dto, boolean showSubmitButton, AppEnv appEnv, int tabIndex,
                                      EnumerationComponentOperation enumerationComponentOperation,
                                      EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, enumerationComponentOperation.getSaveAction(null, tabIndex),
                showSubmitButton, appEnv, entityNewComponentOperation);
        this.tabIndex = tabIndex;
        this.enumerationComponentOperation = enumerationComponentOperation;
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = Transl.get(getDto().getEnumerationTypeDto().getTranslationKey())
                .concat(" - ")
                .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE + "/" + tabIndex);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(getDto().getEnumerationTypeDto().getId().concat("-detail-card-rf"));
        this.setSizeFull();

        this.add(new EnumerationDetailTabComponent(this, false, enumerationComponentOperation));
    }
}
