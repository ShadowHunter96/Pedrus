package cz.bbn.cerberus.subject;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.ares.AresService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionSingleListener;
import cz.bbn.cerberus.ico.dto.IcoDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.dto.SubjectFilterDto;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.subject.ui.SubjectView;
import cz.bbn.cerberus.subject.ui.component.SubjectFilterComponent;
import cz.bbn.cerberus.subject.ui.component.SubjectNewDialog;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Component
@Slf4j
public class SubjectComponentOperation {

    private final SubjectService subjectService;
    private final CustomPermissionService customPermissionService;
    private final AresService aresService;
    private final UserService userService;
    private final AppEnv appEnv;

    public SubjectComponentOperation(SubjectService subjectService, CustomPermissionService customPermissionService,
                                     AresService aresService, UserService userService, AppEnv appEnv) {
        this.subjectService = subjectService;
        this.customPermissionService = customPermissionService;
        this.aresService = aresService;
        this.userService = userService;
        this.appEnv = appEnv;
    }

    public SaveAction<SubjectDto> getSaveAction(CustomPermissionSingleListener listener,
                                                SubjectDto dto, SubjectNewDialog subjectNewDialog) {
        return ((newDto, originalDto) -> {
            try {
                save(newDto, originalDto, listener, dto, subjectNewDialog);
            } catch (SystemException e) {
                log.error(TextValues.SYSTEM_EXCEPTION, e);
                ErrorNotification.show(e, appEnv);
            }
        });
    }

    public SaveAction<SubjectDto> getUpdateAction() {
        return ((newDto, originalDto) -> {
            try {
                subjectService.updateSubject(newDto, originalDto);
            } catch (SystemException e) {
                log.error(TextValues.SYSTEM_EXCEPTION, e);
                ErrorNotification.show(e, appEnv);
            }
        });
    }

    public List<UserDto> getUserList() {
        return userService.findUserList();
    }

    public ItemsAction<SubjectDto> getItemsAction(SubjectFilterComponent subjectFilterComponent) {
        return (query, orderList) -> {
            SubjectFilterDto subjectFilterDto = subjectFilterComponent.getSubjectFilterDto();
            subjectFilterDto.setPage(query.getPage());
            subjectFilterDto.setSize(query.getPageSize());
            subjectFilterDto.setOrderList(orderList);
            return subjectService.findSubjectPage(subjectFilterDto);
        };
    }

    public void linkSubject(Set<SubjectDto> subjectDtoSet, String objectId, ObjectType objectType) {
        subjectService.linkSubject(subjectDtoSet, objectId, objectType);
    }

    public ItemsAction<SubjectDto> getItemActionSubjectByObjectPage(String objectId, ObjectType objectType) {
        return (query, orderList) ->
                subjectService.getSubjectByObjectPage(objectId, objectType, query.getPage(), query.getPageSize());
    }

    public DeleteAction getDeleteActionSubjectByObject(String objectId, ObjectType objectType) {
        return id -> subjectService.deleteSubjectByObject(id, objectId, objectType);
    }

    public List<IcoDto> getIcoDtoList(String ico, String name) {
        try {
            return aresService.getIcoList(ico, name);
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(e, appEnv);
        }
        return new ArrayList<>();
    }

    public IcoDto fillDataFromAres(IcoDto icoDto) {
        try {
            return aresService.getDataFromAres(icoDto);
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(e, appEnv);
        }
        return icoDto;
    }

    public List<String> getIdByIcoOrDic(String ico, String dic) {
        return subjectService.getIdByIcoOrDic(ico, dic);
    }

    public SubjectDto updateDataFromAres(SubjectDto subjectDto) {
        return subjectService.updateDataFromAres(subjectDto);
    }

    public SubjectDto setIcoDataToSubject(SubjectDto subjectDto, IcoDto icoDto) {
        return subjectService.setIcoDataToSubject(subjectDto, icoDto);
    }

    public List<SubjectDto> getMySubjects() {
        return subjectService.getMySubjects();
    }

    private void save(SubjectDto newDto, SubjectDto originalDto, CustomPermissionSingleListener listener,
                      SubjectDto dto, SubjectNewDialog subjectNewDialog) throws SystemException {
        if (originalDto.getId() != null) {
            subjectService.updateSubject(newDto, originalDto);
            processCustomPerm(listener, dto);
            UI.getCurrent().navigate(SubjectView.ROUTE);
        } else {
            subjectService.saveSubject(newDto);
            if (subjectNewDialog != null) {
                subjectNewDialog.showWarning(false);
                subjectNewDialog.close();
                UI.getCurrent().navigate(SubjectDetailView.ROUTE.concat("/").concat(newDto.getId()));
            }
        }
        SuccessNotification.showSavingSuccess(appEnv);
    }

    private void processCustomPerm(CustomPermissionSingleListener listener, SubjectDto dto) {
        Set<PermUserDto> userSet = listener.getSinglePermissionUserList();
        if (userSet != null) {
            Set<CustomUserPermissionDto> tempSet = new HashSet<>();
            for (PermUserDto user : userSet) {
                if (!user.isPermanent()) {
                    CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                            DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), Permission.SUBJECT_VIEW.name(),
                            user.getId(), dto.getId(), true
                    );
                    tempSet.add(customUserPermissionDto);
                }
            }
            customPermissionService.saveByUser(tempSet, DomainEnum.SUBJECT_DOMAIN_NAME.getValue(),
                    dto.getId(), Permission.SUBJECT_VIEW.name());
        }
    }

    public Map<Long, String> getOwnerMap() {
        return userService.getOwnerMap();
    }

    public String getOwnNameCompany() {
        return subjectService.getOwnCompanyName();
    }
}
