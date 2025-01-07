package cz.bbn.cerberus.opportunity.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.offer.OfferComponentOpperation;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.ui.component.OfferNewDialog;
import cz.bbn.cerberus.opportunity.OpportunityComponentOperation;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.subject.ui.component.SubjectGridComponent;
import cz.bbn.cerberus.subject.ui.component.SubjectLinkDialog;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;

import java.util.List;

public class OpportunityTabsComponent extends TabsComponent<TabSimpleComponent> {

    public OpportunityTabsComponent(String title, List<TabEntry> tabEntryList,
                                    OpportunityDto dto, AppEnv appEnv,
                                    OpportunityComponentOperation componentOperation,
                                    Button addContactPersonButton, boolean hasCustomEditPermission,
                                    AppInfiniteGrid<OfferDto> offerGrid,
                                    OfferComponentOpperation offerComponentOpperation,
                                    UserService userService, ListService listService,
                                    SubjectComponentOperation subjectComponentOperation,
                                    SubjectGridComponent tabSubjectGridComponent, Button addDocument,
                                    EntityNewComponentOperation entityNewComponentOperation,
                                    Button linkActivity, Button linkEmployee, Button addAreaTechnologySign,
                                    SubjectService subjectService,
                                    ContractComponentOperation contractComponentOperation) {
        super(title, tabEntryList, entityNewComponentOperation);

        if (SecurityUtils.hasPermission(Permission.OPPORTUNITY_CREATE_CONTRACT) && dto.getProgress() > 99
                && hasCustomEditPermission) {
            Button createContract = VaadinComponents.getNewButton(Transl.get("Add contract"), false);
            createContract.addClickListener(buttonClickEvent -> {
                CreateContractDialogComponent createContractDialogComponent = new CreateContractDialogComponent(
                        dto, componentOperation, listService.getContractTypeDtoList(), appEnv,
                        listService, contractComponentOperation);
                createContractDialogComponent.open();
            });
            addToButtonFooter(createContract);
        }
        if (hasCustomEditPermission && SecurityUtils.hasPermission(Permission.OFFER_EDIT)) {
            Button newOffer = VaadinComponents.getNewButton(Transl.get("Add Offer"), false);
            newOffer.addClickListener(buttonClickEvent -> {
                OfferNewDialog dialog = new OfferNewDialog(
                        offerGrid, offerComponentOpperation, dto, userService, listService, appEnv, subjectService);
                dialog.open();
            });
            addToButtonFooter(newOffer);
        }
        if (hasCustomEditPermission && SecurityUtils.hasPermission(Permission.DOCUMENT_EDIT)) {
            addToButtonFooter(addDocument);
        }

        if (hasCustomEditPermission && SecurityUtils.hasPermission(Permission.OPPORTUNITY_TECHNOLOGY_SIGN_EDIT)) {
            addToButtonFooter(addAreaTechnologySign);
        }

        if (hasCustomEditPermission && SecurityUtils.hasCustomPermission(DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(),
                dto.getId(), Permission.OPPORTUNITY_LINK_CONTACT_PERSON.name())) {
            addToButtonFooter(addContactPersonButton);
        }

        if (hasCustomEditPermission) {
            Button linkSubSupplier = VaadinComponents.getLinkButton(Transl.get("Link subsupplier"));
            linkSubSupplier.addClickListener(buttonClickEvent -> {
                SubjectLinkDialog dialog =
                        new SubjectLinkDialog(Transl.get("Link subsupplier"), subjectComponentOperation,
                                appEnv, dto.getId(), ObjectType.OPPORTUNITY, tabSubjectGridComponent);
                dialog.open();
            });
            addToButtonFooter(linkSubSupplier);
        }

        if (SecurityUtils.hasPermission(Permission.ACTIVITY_BY_OBJECT_LINK) && hasCustomEditPermission) {
            this.addToFooter(linkActivity);
        }
        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_BY_OBJECT_LINK) && hasCustomEditPermission) {
            this.addToFooter(linkEmployee);
        }

        addBackButton();
        if (hasCustomEditPermission) {
            addSaveButton();
        }

    }

}
