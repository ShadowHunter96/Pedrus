package cz.bbn.cerberus.email;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.email.dto.EmailDto;
import cz.bbn.cerberus.email.dto.EmailFilterDto;
import cz.bbn.cerberus.email.dto.EmailSimpleDto;
import cz.bbn.cerberus.email.dto.SimpleItemDto;
import cz.bbn.cerberus.email.ui.component.EmailFilterComponent;
import cz.bbn.cerberus.email.ui.component.EmailGridComponent;
import cz.bbn.cerberus.email.ui.component.EmailImportDialog;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

@Slf4j
@Component
public class EmailComponentOperations {

    private final EmailService emailService;
    private final ListService listService;

    private final AppEnv appEnv;

    public EmailComponentOperations(EmailService emailService, ListService listService, AppEnv appEnv) {
        this.emailService = emailService;
        this.listService = listService;
        this.appEnv = appEnv;
    }

    public ItemsAction<EmailSimpleDto> getItemsAction(EmailFilterComponent emailFilterComponent) {
        return (query, orderList) -> {
            if (orderList.isEmpty()) {
                orderList.add(Sort.Order.desc("dateAndTime"));
            }
            EmailFilterDto filter = emailFilterComponent.getEmailFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return emailService.findEmailDtoPage(filter);
        };
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                emailService.deleteEmail(Long.valueOf(id));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getOpenImportClickEvent(
            EmailGridComponent grid) {
        return buttonClickEvent -> new EmailImportDialog(this, grid, appEnv).open();
    }

    public List<SubjectDto> getCustomerList() {
        List<SubjectDto> toReturnList = new ArrayList<>();
        Set<String> allowedSubjects = SecurityUtils.getAllowedEntityIdByDomain(
                Permission.SUBJECT_EDIT.getAuthority(), DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
        List<SubjectDto> allSubjects = listService.getSubjectDtoList();
        for (SubjectDto subject : allSubjects) {
            if (Boolean.TRUE.equals(subject.getCustomer()) && allowedSubjects.contains(subject.getId())) {
                toReturnList.add(subject);
            }
        }
        toReturnList.sort(Comparator.comparing(SubjectDto::getName));
        return toReturnList;
    }

    public List<DomainEnum> getSubjectDomainEnum() {
        return Arrays.asList(DomainEnum.OFFER_DOMAIN_NAME, DomainEnum.OPPORTUNITY_DOMAIN_NAME,
                DomainEnum.CONTRACT_DOMAIN_NAME, DomainEnum.PROJECT_DOMAIN_NAME, DomainEnum.SUBJECT_DOMAIN_NAME);
    }

    public List<SimpleItemDto> getSimpleItems(DomainEnum domain, String subjectId) {
        switch (domain) {
            case OFFER_DOMAIN_NAME -> {
                return getOfferSimpleItems(subjectId);
            }
            case OPPORTUNITY_DOMAIN_NAME -> {
                return getOpportunitySimpleItems(subjectId);
            }
            case CONTRACT_DOMAIN_NAME -> {
                return getContractSimpleItems(subjectId);
            }
            case PROJECT_DOMAIN_NAME -> {
                return getProjectSimpleItems(subjectId);
            }
            case SUBJECT_DOMAIN_NAME -> {
                return getCustomerSimpleItems();
            }
            default -> {
                return new ArrayList<>();
            }
        }
    }

    public SaveAction<EmailDto> getSaveAction(AppInfiniteGrid<EmailSimpleDto> grid, AppDialog dialog) {
        return (dto, originalDto) -> {
            try {
                emailService.saveEmail(dto);
                dialog.close();
                SuccessNotification.showSavingSuccess(appEnv);
                grid.loadData();
            } catch (SystemException e) {
                log.error(e.getMessage(), e);
                ErrorNotification.show(Transl.get(e.getMessage()), appEnv);
            }
        };
    }

    public EmailDto getEmailDto(Long id) throws SystemException {
        return emailService.getEmailDto(id);
    }

    private List<SimpleItemDto> getOfferSimpleItems(String subjectId) {
        List<SimpleItemDto> simpleItemList = new ArrayList<>();
        List<OfferDto> allOfferList = listService.getOfferDtoList();
        Set<String> allowedOfferSet = SecurityUtils.getAllowedEntityIdByDomain(
                Permission.OFFER_EMAIL_EDIT.getAuthority(), DomainEnum.OFFER_DOMAIN_NAME.getValue());
        for (OfferDto offerDto : allOfferList) {
            if (allowedOfferSet.contains(offerDto.getId()) && offerDto.getSubjectDto() != null
                    && subjectId.equals(offerDto.getOpportunityDto().getSubject().getId())) {
                simpleItemList.add(new SimpleItemDto(offerDto.getId(), offerDto.getName()));
            }
        }
        simpleItemList.sort(Comparator.comparing(SimpleItemDto::getName));
        return simpleItemList;
    }

    public AppEnv egtAppEnv() {
        return appEnv;
    }

    private List<SimpleItemDto> getOpportunitySimpleItems(String subjectId) {
        List<SimpleItemDto> simpleItemList = new ArrayList<>();
        Set<String> allowedOpportunitySet = SecurityUtils.getAllowedEntityIdByDomain(
                Permission.OPPORTUNITY_EMAIL_EDIT.getAuthority(), DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue());
        List<OpportunityDto> allOpportunityList = listService.getOpportunityDtoList();
        for (OpportunityDto opportunityDto : allOpportunityList) {
            if (allowedOpportunitySet.contains(opportunityDto.getId()) && opportunityDto.getSubject() != null
                    && subjectId.equals(opportunityDto.getSubject().getId())) {
                simpleItemList.add(new SimpleItemDto(opportunityDto.getId(), opportunityDto.getName()));
            }
        }
        simpleItemList.sort(Comparator.comparing(SimpleItemDto::getName));
        return simpleItemList;
    }

    private List<SimpleItemDto> getContractSimpleItems(String subjectId) {
        List<SimpleItemDto> simpleItemList = new ArrayList<>();
        Set<String> allowedContractSet = SecurityUtils.getAllowedEntityIdByDomain(
                Permission.CONTRACT_EMAIL_EDIT.getAuthority(), DomainEnum.CONTRACT_DOMAIN_NAME.getValue());
        List<ContractDto> allContractList = listService.getContractList();
        for (ContractDto contractDto : allContractList) {
            if (allowedContractSet.contains(contractDto.getId()) && contractDto.getSubjectDto() != null
                    && subjectId.equals(contractDto.getSubjectDto().getId())) {
                simpleItemList.add(new SimpleItemDto(contractDto.getId(), contractDto.getName()));
            }
        }
        simpleItemList.sort(Comparator.comparing(SimpleItemDto::getName));
        return simpleItemList;
    }

    private List<SimpleItemDto> getProjectSimpleItems(String subjectId) {
        List<SimpleItemDto> simpleItemList = new ArrayList<>();
        Set<String> allowedProjectSet = SecurityUtils.getAllowedEntityIdByDomain(
                Permission.PROJECT_EMAIL_EDIT.getAuthority(), DomainEnum.PROJECT_DOMAIN_NAME.getValue());
        List<ProjectDto> allProjectList = listService.getProjectDtoList();
        for (ProjectDto projectDto : allProjectList) {
            if (allowedProjectSet.contains(projectDto.getId()) && projectDto.getSubject() != null
                    && subjectId.equals(projectDto.getSubject().getId())) {
                simpleItemList.add(new SimpleItemDto(projectDto.getId(), projectDto.getName()));
            }
        }
        simpleItemList.sort(Comparator.comparing(SimpleItemDto::getName));
        return simpleItemList;
    }

    private List<SimpleItemDto> getCustomerSimpleItems() {
        List<SimpleItemDto> toReturnList = new ArrayList<>();
        Set<String> allowedSubjects = SecurityUtils.getAllowedEntityIdByDomain(
                Permission.SUBJECT_EMAIL_EDIT.getAuthority(), DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
        List<SubjectDto> allSubjects = listService.getSubjectDtoList();
        for (SubjectDto subject : allSubjects) {
            if (Boolean.TRUE.equals(subject.getCustomer()) && allowedSubjects.contains(subject.getId())) {
                toReturnList.add(new SimpleItemDto(subject.getId(), subject.getName()));
            }
        }
        toReturnList.sort(Comparator.comparing(SimpleItemDto::getName));
        return toReturnList;
    }

    public String getObjectName(EmailSimpleDto item) {
        return listService.getNameByIdAndObjectType(item.getEntityId(), item.getEntityType());
    }

    public String getDeletePermByDomain(String entityType) {
        DomainEnum domain = DomainEnum.getDomainByValue(entityType);
        String permToReturn;
        switch (domain) {
            case OFFER_DOMAIN_NAME -> permToReturn = Permission.OFFER_EMAIL_DELETE.name();
            case OPPORTUNITY_DOMAIN_NAME -> permToReturn = Permission.OPPORTUNITY_EMAIL_DELETE.name();
            case CONTRACT_DOMAIN_NAME -> permToReturn = Permission.CONTRACT_EMAIL_DELETE.name();
            case PROJECT_DOMAIN_NAME -> permToReturn = Permission.PROJECT_EMAIL_DELETE.name();
            case SUBJECT_DOMAIN_NAME -> permToReturn = Permission.SUBJECT_EMAIL_DELETE.name();
            default -> permToReturn = "";
        }
        return permToReturn;
    }
}
