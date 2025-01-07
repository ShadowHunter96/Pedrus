package cz.bbn.cerberus.opportunity;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.area.AreaComponentOperation;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.contactperson.ContactPersonService;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectFilterDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonObjectTypeEnum;
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionSingleListener;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.opportunity.ui.OpportunityDetailView;
import cz.bbn.cerberus.opportunity.ui.component.CreateContractDialogComponent;
import cz.bbn.cerberus.opportunity.ui.component.OpportunityNewDialog;
import cz.bbn.cerberus.opportunity.ui.component.tabs.OpportunityContactPersonTab;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.technology.TechnologyComponentOperation;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
@Slf4j
public class OpportunityComponentOperation {

    private final AppEnv appEnv;
    private final OpportunityService opportunityService;
    private final ContactPersonService contactPersonService;
    private final ContractService contractService;
    private final UserService userService;
    private final CustomPermissionService customPermissionService;
    private final TechnologyComponentOperation technologyComponentOperation;
    private final AreaComponentOperation areaComponentOperation;

    public OpportunityComponentOperation(AppEnv appEnv, OpportunityService opportunityService,
                                         ContactPersonService contactPersonService,
                                         ContractService contractService, UserService userService,
                                         CustomPermissionService customPermissionService,
                                         TechnologyComponentOperation technologyComponentOperation,
                                         AreaComponentOperation areaComponentOperation) {
        this.appEnv = appEnv;
        this.opportunityService = opportunityService;
        this.contactPersonService = contactPersonService;
        this.contractService = contractService;
        this.userService = userService;
        this.customPermissionService = customPermissionService;
        this.technologyComponentOperation = technologyComponentOperation;
        this.areaComponentOperation = areaComponentOperation;
    }

    public SaveAction<OpportunityDto> getSaveAction(CustomPermissionSingleListener listener,
                                                    OpportunityNewDialog opportunityNewDialog) {
        return (newDto, originalDto) -> {
            try {
                save(newDto, originalDto, opportunityNewDialog, listener);

            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public SaveAction<ContractDto> getSaveActionCreateContract(
            CreateContractDialogComponent createContractDialogComponent) {
        return (newDto, originalDto) -> {
            try {
                contractService.saveContractFromOpportunity(newDto);
                createContractDialogComponent.close();
                SuccessNotification.show(Transl.get("Contract created successfully"), appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }


    public DeleteAction getContactPersonRemoveAction(
            OpportunityContactPersonTab opportunityContactPersonTab, OpportunityDto dto) {
        return id -> {
            contactPersonService.deleteAddedContactPerson(id, ContactPersonObjectTypeEnum.OPORTUNITY, dto.getId());
            SuccessNotification.showDeleteSuccess(appEnv);
            opportunityContactPersonTab.getGrid().loadData();
        };
    }

    public ItemsAction<ContactPersonByObjectDto> getContactPersonItemsAction(OpportunityDto dto) {
        return (query, orderList) -> {
            ContactPersonByObjectFilterDto filterDto = new ContactPersonByObjectFilterDto();
            filterDto.setPage(query.getPage());
            filterDto.setSize(query.getPageSize());
            filterDto.setOrderList(orderList);
            filterDto.setObjectType(ContactPersonObjectTypeEnum.OPORTUNITY);
            filterDto.setObjectId(dto.getId());
            return contactPersonService.findSubjectContactPageByObjectPage(filterDto);
        };
    }

    public List<ContractDto> getContractList() {
        return contractService.getMyInvoiceEditContractList();
    }

    public List<UserDto> getUserList() {
        return userService.findUserList();
    }

    public List<AreaDto> getAreaDtoList() {
        return areaComponentOperation.findAllAreaDtoList();
    }

    public List<TechnologyDto> getTechnologyDtoList() {
        return technologyComponentOperation.findAllowedTechnologyList();
    }

    private void save(OpportunityDto newDto, OpportunityDto originalDto, OpportunityNewDialog opportunityNewDialog,
                      CustomPermissionSingleListener listener) throws SystemException {
        if (opportunityNewDialog == null && StringUtils.isNoneEmpty(newDto.getId())) {
            opportunityService.updateOpportunity(newDto, originalDto);
            processCustomPerms(listener, newDto);

            UI.getCurrent().getPage().getHistory().back();
        } else {
            newDto.setDeleted(false);
            opportunityService.saveOpportunity(newDto);
            if (opportunityNewDialog != null) {
                opportunityNewDialog.showWarning(false);
                opportunityNewDialog.close();
                UI.getCurrent().navigate(OpportunityDetailView.ROUTE.concat("/")
                        .concat(newDto.getId().replace("/", "&ndash")));
            }
        }

        SuccessNotification.showSavingSuccess(appEnv);
    }

    private void processCustomPerms(CustomPermissionSingleListener listener, OpportunityDto newDto) {
        Set<PermUserDto> userSet = listener.getSinglePermissionUserList();
        if (userSet != null) {
            Set<CustomUserPermissionDto> tempSet = new HashSet<>();
            for (PermUserDto user : userSet) {
                if (!user.isPermanent()) {
                    CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                            DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), Permission.OPPORTUNITY_VIEW.name(),
                            user.getId(), newDto.getId(), true
                    );
                    tempSet.add(customUserPermissionDto);
                }
            }
            customPermissionService.saveByUser(tempSet, DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(),
                    newDto.getId(), Permission.OPPORTUNITY_VIEW.name());
        }
    }
}
