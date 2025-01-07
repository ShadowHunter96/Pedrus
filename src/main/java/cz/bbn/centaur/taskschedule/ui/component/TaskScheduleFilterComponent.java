package cz.bbn.cerberus.taskschedule.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.AppAdvancedFilter;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleFilterDto;
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleFrequency;
import cz.bbn.cerberus.tasktemplate.ui.TaskTemplateView;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.List;
import java.util.Map;

public class TaskScheduleFilterComponent extends AppAdvancedFilter {

    private IntegerField id;
    private TextField search;
    private Checkbox showAssignedToMe;
    private Checkbox showDeleted;
    private MultiSelectComboBox<UserDto> assignedUsers;
    private ComboBox<ObjectType> entityType;
    private TextField entityId;
    private ComboBox<TaskScheduleFrequency> frequency;
    private ComboBox<UserDto> owner;
    private ComboBox<TaskTypeDto> taskType;

    private final Button searchButton;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final List<UserDto> userList;
    private final List<TaskTypeDto> taskTypeList;

    public TaskScheduleFilterComponent(Button searchButton, String params, HistoryBreadcrumbs historyBreadcrumbs,
                                       List<UserDto> userList, List<TaskTypeDto> taskTypeList) {
        super(searchButton);
        this.params = params;
        this.searchButton = searchButton;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.userList = userList;
        this.taskTypeList = taskTypeList;
        initComponent();
        fillFilterFromUrl();
    }

    private void initComponent() {

        id = new IntegerField(Transl.get("Id"));
        addToBasicFilter(id);

        search = new TextField(Transl.get("Search"));
        addToBasicFilter(search);

        assignedUsers = new MultiSelectComboBox<>(Transl.get("Assigned user"));
        assignedUsers.setItems(userList);
        assignedUsers.setItemLabelGenerator(UserDto::getName);
        addToBasicFilter(assignedUsers);

        showAssignedToMe = new Checkbox(Transl.get("Show assigned to me"));
        showAssignedToMe.setValue(false);
        addToBasicFilter(showAssignedToMe);

        entityType = new ComboBox<>(Transl.get("Object type"));
        entityType.setItems(ObjectType.values());
        entityType.setItemLabelGenerator(this::getTranslatedObjectType);
        addToBasicFilter(entityType);

        frequency = new ComboBox<>(Transl.get("Frequency"));
        frequency.setItems(TaskScheduleFrequency.values());
        frequency.setItemLabelGenerator(TaskScheduleFrequency::getTranslatedValue);
        addToBasicFilter(frequency);

        taskType = new ComboBox<>(Transl.get("Task type"));
        taskType.setItems(taskTypeList);
        taskType.setItemLabelGenerator(TaskTypeDto::getName);
        addToBasicFilter(taskType);

        owner = new ComboBox<>(Transl.get("Owner"));
        owner.setItems(userList);
        owner.setItemLabelGenerator(UserDto::getName);
        addToAdvancedFilter(owner);

        assignedUsers = new MultiSelectComboBox<>(Transl.get("Assigned users"));
        assignedUsers.setItems(userList);
        assignedUsers.setItemLabelGenerator(UserDto::getName);
        addToAdvancedFilter(assignedUsers);

        entityId = new TextField(Transl.get("Object id"));
        addToAdvancedFilter(entityId);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        showDeleted.setValue(false);
        addToAdvancedFilter(showDeleted);

        addToBasicFilter(searchButton);

        initFilter();

    }

    public TaskScheduleFilterDto getTaskScheduleFilterDto() {
        TaskScheduleFilterDto taskScheduleFilterDto = new TaskScheduleFilterDto();
        taskScheduleFilterDto.setId(id.getValue() != null ? id.getValue().longValue() : null);
        taskScheduleFilterDto.setSearch(search.getValue());
        taskScheduleFilterDto.setShowMine(showAssignedToMe.getValue());
        taskScheduleFilterDto.setShowDeleted(showDeleted.getValue());
        taskScheduleFilterDto.setAssignedUserSet(assignedUsers.getValue());
        taskScheduleFilterDto.setObjectType(entityType.getValue());
        taskScheduleFilterDto.setFrequency(frequency.getValue());
        taskScheduleFilterDto.setTaskType(taskType.getValue());
        if (owner.getValue() != null) {
            taskScheduleFilterDto.setOwner(owner.getValue().getId());
        }
        taskScheduleFilterDto.setObjectId(entityId.getValue());
        return taskScheduleFilterDto;
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

    private String getTranslatedObjectType(ObjectType objectTypeTemp) {
        return Transl.get(objectTypeTemp.name());
    }
}
