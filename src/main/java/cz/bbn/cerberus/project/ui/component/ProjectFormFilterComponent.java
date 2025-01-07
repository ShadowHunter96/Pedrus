package cz.bbn.cerberus.project.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.ProjectState;
import cz.bbn.cerberus.project.dto.ProjectFilterDto;
import cz.bbn.cerberus.project.ui.ProjectView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.Map;

@Slf4j
public class ProjectFormFilterComponent extends FormLayout {

    private TextField id;
    private TextField name;
    private TextField subject;
    private Checkbox showDeleted;
    private ComboBox<UserDto> user;
    private ComboBox<ProjectState> state;
    private Checkbox onlyEditPermission;

    private final List<UserDto> userList;
    private final Button search;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;

    public ProjectFormFilterComponent(Button search, List<UserDto> userList, String params,
                                      HistoryBreadcrumbs historyBreadcrumbs) {
        this.params = params;
        this.search = search;
        this.userList = userList;
        this.historyBreadcrumbs = historyBreadcrumbs;
        initComponents();
    }

    private void initComponents() {
        id = new TextField(Transl.get("Id"));
        this.add(id);

        name = new TextField(Transl.get("Name"));
        this.add(name);

        subject = new TextField(Transl.get("Subject"));
        this.add(subject);

        user = new ComboBox<>(Transl.get("Owner"));
        UserDto defaultUserDto = new UserDto(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE));
        userList.add(0, defaultUserDto);
        user.setItems(userList);
        user.setItemLabelGenerator(UserDto::getName);
        user.setValue(defaultUserDto);
        this.add(user);

        state = new ComboBox<>(Transl.get("State"));
        state.setItems(ProjectState.values());
        state.setItemLabelGenerator(actualProjectState -> Transl.get(actualProjectState.name()));
        this.add(state);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        if (SecurityUtils.hasPermission(Permission.PROJECT_SHOW_DELETED)) {
            this.add(showDeleted);
        }

        onlyEditPermission = new Checkbox(Transl.get("Only editable"));
        onlyEditPermission.setValue(true);
        this.add(onlyEditPermission);

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
        fillFilterFromUrl();
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("id")) {
            id.setValue(map.get("id"));
        }
        if (map.containsKey("name")) {
            name.setValue(map.get("name"));
        }
        if (map.containsKey("subject")) {
            subject.setValue(map.get("subject"));
        }
        if (map.containsKey("userId")) {
            UserDto userDto = userList.stream().filter(actualUserDto ->
                    actualUserDto.getId().equals(Long.valueOf(map.get("userId")))).findAny().orElse(null);
            user.setValue(userDto);
        }
        if (map.containsKey("state")) {
            state.setValue(ProjectState.valueOf(map.get("state")));
        }
        if (map.containsKey("showDeleted") && SecurityUtils.hasPermission(Permission.PROJECT_SHOW_DELETED)) {
            showDeleted.setValue(Boolean.valueOf(map.get("showDeleted")));
        }
        if (map.containsKey("onlyEditPermission")) {
            onlyEditPermission.setValue(true);
        }
    }

    public void fillUrl() {
        String paramUrl = ProjectView.ROUTE.concat("/");
        if (!id.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&id=").concat(id.getValue());
        }
        if (!name.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&name=").concat(name.getValue());
        }
        if (!subject.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&subject=").concat(subject.getValue());
        }
        if (user.getValue() != null) {
            paramUrl = paramUrl.concat("&userId=").concat(String.valueOf(user.getValue().getId()));
        }
        if (state.getValue() != null) {
            paramUrl = paramUrl.concat("&state=").concat(state.getValue().name());
        }
        if (onlyEditPermission.getValue() != null) {
            paramUrl = paramUrl.concat("&onlyEditPermission=").concat(onlyEditPermission.getValue().toString());
        }
        if (showDeleted.getValue() != null && SecurityUtils.hasPermission(Permission.PROJECT_SHOW_DELETED)) {
            paramUrl = paramUrl.concat("&showDeleted=").concat(showDeleted.getValue().toString());
        }

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    public ProjectFilterDto getProjectFilterDto() {
        ProjectFilterDto dto = new ProjectFilterDto();
        dto.setId(id.getValue());
        dto.setName(name.getValue());
        dto.setSubject(subject.getValue());
        dto.setShowDeleted(showDeleted.getValue());
        dto.setUserDto(user.getValue());
        dto.setState(state.getValue());
        dto.setOnlyEditPermission(onlyEditPermission.getValue());
        return dto;
    }

}
