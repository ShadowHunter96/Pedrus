package cz.bbn.cerberus.custompermission.ui;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.custompermission.dto.ObjectUserPermissionDto;
import cz.bbn.cerberus.custompermission.ui.component.CustomPermissionTabComponent;
import cz.bbn.cerberus.custompermission.ui.component.CustomPermissionTabsComponent;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Route(value = CustomPermissionView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.CUSTOM_PERMISSION_VIEW)
@Slf4j
public class CustomPermissionView extends AppView implements CustomPermissionViewListener {

    public static final String ROUTE = "custom-permission-view";

    private final CustomPermissionService permissionService;
    private final ListService listService;
    private final UserService userService;
    private final EntityNewComponentOperation entityNewComponentOperation;

    private Set<ObjectUserPermissionDto> combList;

    public CustomPermissionView(CustomPermissionService permissionService,
                                ListService listService, AppEnv appEnv,
                                UserService userService, EntityNewComponentOperation entityNewComponentOperation) {
        this.permissionService = permissionService;
        this.listService = listService;
        this.userService = userService;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView() {
        combList = new HashSet<>();
        setSizeFull();
        List<UserDto> userList = userService.findUserList();

        List<TabEntry> tabList = new ArrayList<>();

        for (Map.Entry<String, List<String>> entry : SecurityUtils.getObjectMap().entrySet()) {
            Set<String> permissionSet = SecurityUtils.getCustomPermissionByObjectName(entry.getKey());
            tabList.add(new TabEntry(Transl.get(entry.getKey()), new CustomPermissionTabComponent(
                    this, permissionSet, entry.getValue(), userList, entry.getKey())));
        }
        add(new CustomPermissionTabsComponent(this, Transl.get("Custom permissions"), tabList,
                entityNewComponentOperation));
    }

    @Override
    public void setCustomPermissionDtoList(Set<CustomUserPermissionDto> customPermissionSet, String object,
                                           UserDto user, String objectId) {
        combList.removeIf(oup -> oup.getObject().equals(object) && oup.getUser().equals(user));
        combList.add(new ObjectUserPermissionDto(user, object, customPermissionSet, objectId));
    }

    @Override
    public Set<CustomUserPermissionDto> getUserPermissions(String object, UserDto user, String objectId) {
        for (ObjectUserPermissionDto objDto : combList) {
            if (objDto.getUser().getId().equals(user.getId()) && objDto.getObject().equals(object)
                    && objDto.getObjectId().equals(objectId)) {
                return objDto.getPermissionSet();
            }
        }
        return permissionService.getPermissionByObjectAndUser(object, user.getId(), objectId);
    }

    @Override
    public void saveAll() {
        //permissionService.saveUserPermissions(combList);
        //SuccessNotification.showSavingSuccess(appEnv);
    }

    @Override
    protected void onAttach(AttachEvent event) {
        super.onAttach(event);
        initView();
    }
}
