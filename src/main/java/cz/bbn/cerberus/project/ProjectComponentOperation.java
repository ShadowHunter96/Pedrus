package cz.bbn.cerberus.project;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionSingleListener;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.project.dto.ProjectFilterDto;
import cz.bbn.cerberus.project.dto.ProjectSimpleDto;
import cz.bbn.cerberus.project.ui.ProjectDetailView;
import cz.bbn.cerberus.project.ui.component.ProjectNewDialog;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
@Slf4j
public class ProjectComponentOperation {

    private final ProjectService projectService;
    private final CustomPermissionService customPermissionService;
    private final UserService userService;
    private final ContractService contractService;
    private final AppEnv appEnv;

    public ProjectComponentOperation(ProjectService projectService, UserService userService,
                                     CustomPermissionService customPermissionService,
                                     ContractService contractService,
                                     AppEnv appEnv) {
        this.projectService = projectService;
        this.userService = userService;
        this.customPermissionService = customPermissionService;
        this.contractService = contractService;
        this.appEnv = appEnv;
    }

    public SaveAction<ProjectDto> getSaveAction(
            CustomPermissionSingleListener listener, ProjectDto dto, ProjectNewDialog dialog) {
        return (newDto, originalDto) -> {
            try {
                if (originalDto.getId() != null) {
                    projectService.updateProject(newDto, originalDto);
                    processCustomPerm(listener, dto);
                    UI.getCurrent().getPage().getHistory().back();
                } else {
                    projectService.saveProject(newDto);
                    if (dialog != null) {
                        dialog.showWarning(false);
                        dialog.close();
                        UI.getCurrent().navigate(ProjectDetailView.ROUTE.concat("/").concat(newDto.getId()));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public List<UserDto> getUserList() {
        return userService.findUserList();
    }


    public List<ContractDto> getAllowedContractList() {
        return contractService.getMyInvoiceEditContractList();
    }

    public ItemsAction<ProjectSimpleDto> getItemsAction(String id, boolean contract) {
        return (query, orderList) -> {
            ProjectFilterDto projectFilterDto = new ProjectFilterDto();
            projectFilterDto.setPage(query.getPage());
            projectFilterDto.setSize(query.getPageSize());
            projectFilterDto.setOrderList(orderList);
            projectFilterDto.setObjectId(id);
            if (contract) {
                projectFilterDto.setContract(id);
            } else {
                projectFilterDto.setSubject(id);
            }
            return projectService.findProjectDtoPage(projectFilterDto);
        };
    }

    private void processCustomPerm(CustomPermissionSingleListener listener, ProjectDto dto) {
        Set<PermUserDto> userSet = listener.getSinglePermissionUserList();
        if (userSet != null) {
            Set<CustomUserPermissionDto> tempSet = new HashSet<>();
            for (PermUserDto user : userSet) {
                if (!user.isPermanent()) {
                    CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                            DomainEnum.PROJECT_DOMAIN_NAME.getValue(), Permission.PROJECT_VIEW.name(),
                            user.getId(), dto.getId(), true
                    );
                    tempSet.add(customUserPermissionDto);
                }
            }
            customPermissionService.saveByUser(tempSet, DomainEnum.PROJECT_DOMAIN_NAME.getValue(),
                    dto.getId(), Permission.PROJECT_VIEW.name());
        }
    }
}
