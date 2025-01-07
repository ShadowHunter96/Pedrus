package cz.bbn.cerberus.subject.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectFilterDto;
import cz.bbn.cerberus.subject.dto.SubjectType;
import cz.bbn.cerberus.subject.ui.SubjectView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class SubjectFilterComponent extends FormLayout {

    private final Button search;

    private TextField id;
    private TextField name;
    private TextField description;
    private TextField url;
    private Checkbox showDeleted;
    private MultiSelectComboBox<UserDto> user;
    private MultiSelectComboBox<SubjectType> subjectType;

    private final List<UserDto> userList;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;

    public SubjectFilterComponent(Button search, List<UserDto> userList, String params,
                                  HistoryBreadcrumbs historyBreadcrumbs) {
        this.search = search;
        this.userList = userList;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        initComponents();
        fillFilterFromUrl();
    }

    public SubjectFilterComponent(Button search, List<UserDto> userList, SubjectType actualSubjectType) {
        this.search = search;
        this.userList = userList;
        this.params = null;
        this.historyBreadcrumbs = null;
        initComponents();
        this.subjectType.setValue(actualSubjectType);
    }

    private void initComponents() {
        id = new TextField(Transl.get("Subject shortcut"));
        this.add(id);

        name = new TextField(Transl.get("Name"));
        this.add(name);

        description = new TextField(Transl.get("Description"));
        this.add(description);

        url = new TextField(Transl.get("Subject url"));
        this.add(url);

        user = new MultiSelectComboBox<>(Transl.get("Owner"));
        user.setItems(userList);
        user.setItemLabelGenerator(UserDto::getName);
        this.add(user);

        subjectType = new MultiSelectComboBox<>(Transl.get("Subject type"));
        subjectType.setItems(SubjectType.values());
        subjectType.setItemLabelGenerator(actualSubjectType -> Transl.get(actualSubjectType.name()));
        this.add(subjectType);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        if (SecurityUtils.hasPermission(Permission.PROJECT_SHOW_DELETED)) {
            this.add(showDeleted);
        }

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("id")) {
            id.setValue(map.get("id"));
        }
        if (map.containsKey("name")) {
            name.setValue(map.get("name"));
        }
        if (map.containsKey("description")) {
            description.setValue(map.get("description"));
        }
        if (map.containsKey("userId")) {
            String[] users = map.get("userId").split(":");
            Set<UserDto> userDtoSet = userList.stream().filter(actualUserDto1 ->
                    Arrays.stream(users).toList()
                            .contains(String.valueOf(actualUserDto1.getId()))).collect(Collectors.toSet());
            user.setValue(userDtoSet);
        }
        if (map.containsKey("subjectType")) {
            String[] subjectTypes = map.get("subjectType").split(":");
            Set<SubjectType> subjectTypeSet = Arrays.stream(SubjectType.values()).filter(actual ->
                    Arrays.stream(subjectTypes).collect(Collectors.toSet())
                            .contains(actual.name())).collect(Collectors.toSet());
            subjectType.setValue(subjectTypeSet);
        }
        if (map.containsKey("url")) {
            url.setValue(map.get("url"));
        }
        if (map.containsKey("showDeleted")) {
            showDeleted.setValue(Boolean.valueOf(map.get("showDeleted")));
        }
        if (map.containsKey("onlyCustomer")) {
            subjectType.setValue(SubjectType.CUSTOMER);
        }

    }

    public void fillUrl() {
        String paramUrl = SubjectView.ROUTE.concat("/");
        if (!id.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&id=").concat(id.getValue());
        }
        if (!name.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&name=").concat(name.getValue());
        }
        if (!description.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&description=").concat(description.getValue());
        }
        if (!CollectionUtils.isEmpty(user.getValue())) {
            String userIdsString = StringUtils.join(user.getValue().stream()
                    .map(UserDto::getId)
                    .toList(), ":");
            paramUrl = paramUrl.concat("&userId=").concat(userIdsString);
        }
        if (!CollectionUtils.isEmpty(subjectType.getValue())) {
            String subjectTypeIdsString = StringUtils.join(subjectType.getValue(), ":");
            paramUrl = paramUrl.concat("&subjectType=").concat(subjectTypeIdsString);
        }
        paramUrl = paramUrl.concat("&showDeleted=").concat(showDeleted.getValue().toString());

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    public SubjectFilterDto getSubjectFilterDto() {
        SubjectFilterDto dto = new SubjectFilterDto();
        dto.setId(id.getValue());
        dto.setName(name.getValue());
        dto.setDescription(description.getValue());
        dto.setUrl(url.getValue());
        dto.setShowDeleted(showDeleted.getValue());
        dto.setUserDtoSet(user.getValue());
        dto.setSubjectTypeSet(subjectType.getValue());
        return dto;
    }
}
