package cz.bbn.cerberus.documenttype.ui.component;

import com.vaadin.flow.component.checkbox.CheckboxGroup;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.DocumentTypeTabComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.documenttype.ui.component.tab.DocumentTypeDetailTabComponent;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.translation.Transl;

public class DocumentTypeDetailComponent
        extends AppDetailCardComponent<DocumentTypeDto> implements AppBinderOperation<DocumentTypeDto> {

    private final ListService listService;
    private final CheckboxGroup<EnumerationDto> allowedFormats;

    public DocumentTypeDetailComponent(DocumentTypeDto dto, SaveAction<DocumentTypeDto> saveAction,
                                       boolean showSubmitButton, AppEnv appEnv, ListService listService,
                                       CheckboxGroup<EnumerationDto> allowedFormats,
                                       EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        this.listService = listService;
        this.allowedFormats = allowedFormats;
        initComponent();
    }

    @Override
    protected void initComponent() {
        setSizeFull();
        String heading = getDto().getId() == null ? Transl.get("New document type") :
                Transl.get("Document type")
                        .concat(" - ")
                        .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE.concat("/")
                .concat(String.valueOf(DocumentTypeTabComponent.TAB_INDEX)));
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.DOCUMENT_TYPE_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        DocumentTypeDetailTabComponent documentTypeDetailTabComponent = new DocumentTypeDetailTabComponent(this, false,
                listService.getEnumerationDtoList("DOCUMENT_FORMAT", true), allowedFormats);
        this.add(documentTypeDetailTabComponent);
    }

}
