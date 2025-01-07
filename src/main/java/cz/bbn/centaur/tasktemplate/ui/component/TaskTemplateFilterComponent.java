package cz.bbn.cerberus.tasktemplate.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.tasktemplate.dto.TaskTemplateFilterDto;
import cz.bbn.cerberus.tasktemplate.ui.TaskTemplateView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.List;
import java.util.Map;

public class TaskTemplateFilterComponent extends FormLayout {

    private IntegerField id;
    private TextField search;
    private ComboBox<UserDto> owner;
    private ComboBox<RoleDto> role;
    private Checkbox showOnlyMine;
    private Checkbox showDeleted;

    private final Button searchButton;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final List<UserDto> userList;
    private final List<RoleDto> roleList;

    public TaskTemplateFilterComponent(Button searchButton, String params, HistoryBreadcrumbs historyBreadcrumbs,
                                       List<UserDto> userList, List<RoleDto> roleList) {
        this.params = params;
        this.searchButton = searchButton;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.userList = userList;
        this.roleList = roleList;
        initComponent();
        fillFilterFromUrl();
    }

    private void initComponent() {

        id = new IntegerField(Transl.get("Id"));
        add(id);

        search = new TextField(Transl.get("Search"));
        add(search);

        owner = new ComboBox<>(Transl.get("Owner"));
        owner.setItems(userList);
        owner.setItemLabelGenerator(UserDto::getName);
        add(owner);

        role = new ComboBox<>(Transl.get("Role"));
        role.setItems(roleList);
        role.setItemLabelGenerator(RoleDto::getDescription);
        add(role);

        showOnlyMine = new Checkbox(Transl.get("Show only mine"));
        showOnlyMine.setValue(false);
        add(showOnlyMine);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        showDeleted.setValue(false);
        add(showDeleted);

        add(searchButton);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);

    }

    public TaskTemplateFilterDto getTaskTemplateFilterDto() {
        TaskTemplateFilterDto taskTemplateFilterDto = new TaskTemplateFilterDto();
        taskTemplateFilterDto.setId(id.getValue() != null ? id.getValue().longValue() : null);
        taskTemplateFilterDto.setSearch(search.getValue());
        if (owner.getValue() != null) {
            taskTemplateFilterDto.setOwner(owner.getValue().getId());
        }
        if (role.getValue() != null) {
            taskTemplateFilterDto.setRole(role.getValue().getId());
        }
        taskTemplateFilterDto.setShowMine(showOnlyMine.getValue());
        taskTemplateFilterDto.setShowDeleted(showDeleted.getValue());
        return taskTemplateFilterDto;
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("id")) {
            id.setValue(Integer.valueOf(map.get("id")));
        }
        if (map.containsKey("name")) {
            search.setValue(map.get("name"));
        }

    }

    public void fillUrl() {
        String paramUrl = TaskTemplateView.ROUTE.concat("/");
        if (id.getValue() != null) {
            paramUrl = paramUrl.concat("&id=").concat(String.valueOf(id.getValue()));
        }
        if (!search.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&name=").concat(search.getValue());
        }

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }
}
