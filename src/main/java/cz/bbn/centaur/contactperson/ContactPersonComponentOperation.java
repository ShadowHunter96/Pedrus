package cz.bbn.cerberus.contactperson;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonFilterDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonObjectTypeEnum;
import cz.bbn.cerberus.contactperson.ui.ContactPersonDetailView;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonFilterComponent;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonGetMultipleEvent;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonLinkDialog;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonNewDialog;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeService;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
@Slf4j
public class ContactPersonComponentOperation {

    private final ContactPersonService contactPersonService;
    private final AppEnv appEnv;
    private final ContactPersonTypeService contactPersonTypeService;
    private final SubjectService subjectService;

    public ContactPersonComponentOperation(ContactPersonService contactPersonService, AppEnv appEnv,
                                           ContactPersonTypeService contactPersonTypeService,
                                           SubjectService subjectService) {
        this.contactPersonService = contactPersonService;
        this.appEnv = appEnv;
        this.contactPersonTypeService = contactPersonTypeService;
        this.subjectService = subjectService;
    }

    public List<String> getProjectList(String id) {
        return contactPersonService
                .findObjectIdSetByIdAndObjectType(ContactPersonObjectTypeEnum.PROJECT.toString(), id);
    }

    public List<String> getSubjectList(String id) {
        return contactPersonService
                .findObjectIdSetByIdAndObjectType(ContactPersonObjectTypeEnum.SUBJECT.toString(), id);
    }

    public List<String> getContractList(String id) {
        return contactPersonService
                .findObjectIdSetByIdAndObjectType(ContactPersonObjectTypeEnum.CONTRACT.toString(), id);
    }

    public List<String> getOpportunityList(String id) {
        return contactPersonService
                .findObjectIdSetByIdAndObjectType(ContactPersonObjectTypeEnum.OPORTUNITY.toString(), id);
    }

    public List<ContactPersonTypeDto> getContactPersonTypeDtoList() {
        return contactPersonTypeService.findAll();
    }

    public List<SubjectDto> getContactPersonSubjectDtoList(String contactPersonId) {
        return subjectService.getSubjectByContactPerson(contactPersonId);
    }

    public ItemsAction<ContactPersonDto> getSubjectLinkItemAction(
            ContactPersonFilterComponent filterComponent, String id) {
        return (query, orderList) -> {
            ContactPersonFilterDto filter = filterComponent.getContactPersonFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return contactPersonService.findContactPersonExcludedBySubject(filter, id);
        };
    }

    public ItemsAction<ContactPersonDto> getObjectSubjectLinkItemAction(ContactPersonFilterComponent filterComponent,
                                                                        String subjectId, String id,
                                                                        ContactPersonObjectTypeEnum type) {
        return (query, orderList) -> {
            ContactPersonFilterDto filter = filterComponent.getContactPersonFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return contactPersonService.findContactPersonOnSubjectNotOnObject(filter, subjectId, id, type.name());
        };
    }

    public ContactPersonGetMultipleEvent getLinkContactEvent(AppInfiniteGrid<?> grid, String id,
                                                             ContactPersonObjectTypeEnum type) {
        return (ContactPersonLinkDialog dialog, Button button) -> e -> {
            Set<ContactPersonByObjectDto> byObjectDtoSet = new HashSet<>();
            for (ContactPersonDto contactDto : dialog.getSelected()) {
                ContactPersonByObjectDto byObjectDto = new ContactPersonByObjectDto();
                byObjectDto.setAddedObjectId(id);
                byObjectDto.setObjectType(type);
                byObjectDto.setId(contactDto.getId());
                byObjectDtoSet.add(byObjectDto);
            }
            addContactPersonSetToObject(byObjectDtoSet, dialog);
            grid.loadData();
            button.setEnabled(true);
        };
    }

    private void addContactPersonSetToObject(Set<ContactPersonByObjectDto> dtoSet, ContactPersonLinkDialog dialog) {
        try {
            if (!dtoSet.isEmpty()) {
                contactPersonService.addContactPersonListByObject(dtoSet);
            }
            dialog.close();
            SuccessNotification.showSavingSuccess(appEnv);
        } catch (SystemException e) {
            log.error("Add contact person to project error", e);
            ErrorNotification.show(e, appEnv);
        }
    }

    private void save(ContactPersonDto newDto, ContactPersonDto originalDto,
                      ContactPersonNewDialog contactPersonNewDialog) throws SystemException {
        boolean allowed = contactPersonTypeService.isContactPersonTypeAllowed(newDto.getContactPersonType().getId());
        if (!allowed) {
            ErrorNotification.show(Transl.get("This contact person type is not allowed"), appEnv);
        } else {
            if (contactPersonNewDialog == null && contactPersonService.contactPersonExists(newDto.getId())) {
                contactPersonService.updateContactPerson(newDto, originalDto);
                UI.getCurrent().getPage().getHistory().back();
            } else {
                contactPersonService.saveContactPerson(newDto);
                if (contactPersonNewDialog != null) {
                    contactPersonNewDialog.showWarning(false);
                    contactPersonNewDialog.close();
                    UI.getCurrent().navigate(ContactPersonDetailView.ROUTE.concat("/").concat(newDto.getId()));
                }
            }
            SuccessNotification.showSavingSuccess(appEnv);
        }
    }

    public SaveAction<ContactPersonDto> getSaveAction(ContactPersonNewDialog contactPersonNewDialog) {
        return (newDto, originalDto) -> {
            try {
                save(newDto, originalDto, contactPersonNewDialog);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }
}
