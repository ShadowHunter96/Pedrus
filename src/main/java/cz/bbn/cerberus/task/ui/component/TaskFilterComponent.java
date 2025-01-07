package cz.bbn.cerberus.task.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datetimepicker.DateTimePicker;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.AppAdvancedFilter;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.task.dto.TaskFilterDto;
import cz.bbn.cerberus.task.dto.TaskState;
import cz.bbn.cerberus.task.ui.TaskView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class TaskFilterComponent extends AppAdvancedFilter {

    private IntegerField id;
    private TextField search;
    private ComboBox<UserDto> owner;
    private ComboBox<UserDto> assignee;
    private Checkbox showMeSolver;
    private Checkbox showAssignedToMe;
    private DateTimePicker dueDateFrom;
    private DateTimePicker dueDateTo;
    private ComboBox<ObjectType> entityType;
    private TextField entityId;
    private MultiSelectComboBox<UserDto> assignedUsers;
    private MultiSelectComboBox<TaskState> state;

    private final Button searchButton;
    private final ObjectType objectType;
    private final String objectId;
    private final String subjectId;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final List<UserDto> userDtoList;
    private final boolean meSolverValue;

    public TaskFilterComponent(Button searchButton, String params, HistoryBreadcrumbs historyBreadcrumbs,
                               List<UserDto> userDtoList, boolean meSolverValue) {
        super(searchButton);
        this.params = params;
        this.searchButton = searchButton;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.subjectId = null;
        this.objectType = null;
        this.objectId = null;
        this.userDtoList = userDtoList;
        this.meSolverValue = meSolverValue;
        initComponent();
        fillFilterFromUrl();
    }

    public TaskFilterComponent(Button searchButton, ObjectType objectType, String objectId, String subjectId,
                               List<UserDto> userDtoList, boolean meSolverValue) {
        super(searchButton);
        params = null;
        this.searchButton = searchButton;
        this.objectType = objectType;
        this.objectId = objectId;
        this.subjectId = subjectId;
        this.historyBreadcrumbs = null;
        this.userDtoList = userDtoList;
        this.meSolverValue = meSolverValue;
        initComponent();
    }

    private void initComponent() {

        id = new IntegerField(Transl.get("Id"));

        search = new TextField(Transl.get("Search"));

        state = new MultiSelectComboBox<>(Transl.get("State"));
        state.setItems(TaskState.values());
        state.setItemLabelGenerator(TaskState::getTranslatedValue);

        owner = new ComboBox<>(Transl.get("Created by"));
        owner.setItems(userDtoList);
        owner.setItemLabelGenerator(UserDto::getName);

        assignee = new ComboBox<>(Transl.get("Solver"));
        assignee.setItems(userDtoList);
        assignee.setItemLabelGenerator(UserDto::getName);

        showMeSolver = new Checkbox(Transl.get("Show where I am a solver"));
        showMeSolver.setValue(meSolverValue);

        showAssignedToMe = new Checkbox(Transl.get("Show assigned to me"));

        dueDateFrom = VaadinComponents.getDateTimePicker(Transl.get("Target date from"), null);
        dueDateTo = VaadinComponents.getDateTimePicker(Transl.get("Target date to"), null);

        entityType = new ComboBox<>(Transl.get("Object type"));
        entityType.setItems(ObjectType.values());
        entityType.setItemLabelGenerator(this::getTranslatedObjectType);

        assignedUsers = new MultiSelectComboBox<>(Transl.get("Assigned users"));
        assignedUsers.setItems(userDtoList);
        assignedUsers.setItemLabelGenerator(UserDto::getName);

        entityId = new TextField(Transl.get("Object id"));

        addToBasicFilter(id, search, state, assignee, showMeSolver, showAssignedToMe,
                dueDateFrom, dueDateTo);
        if (objectType == null) {
            addToBasicFilter(entityType);
        }
        addToAdvancedFilter(owner, assignedUsers);
        if (objectId == null) {
            addToAdvancedFilter(entityId);
        }
        addToBasicFilter(searchButton);
        initFilter();
    }

    public TaskFilterDto getTaskFilterDto() {
        TaskFilterDto taskFilterDto = new TaskFilterDto();
        taskFilterDto.setId(id.getValue() != null ? id.getValue().longValue() : null);
        taskFilterDto.setOwner(owner.getValue() != null ? owner.getValue().getId() : null);
        taskFilterDto.setSearch(search.getValue());
        taskFilterDto.setAssignee(assignee.getValue() != null ? assignee.getValue().getId() : null);
        taskFilterDto.setShowMeSolver(showMeSolver.getValue());
        taskFilterDto.setShowAssignedToMe(showAssignedToMe.getValue());
        taskFilterDto.setDateTo(dueDateTo.getValue());
        taskFilterDto.setDateFrom(dueDateFrom.getValue());
        taskFilterDto.setObjectType(objectType != null ? objectType :
                (entityType.getValue() != null ? entityType.getValue() : null));
        taskFilterDto.setObjectId(objectId != null ? objectId :
                (entityId.getValue() != null ? entityId.getValue() : null));
        taskFilterDto.setAssignedUsers(assignedUsers.getValue().stream()
                .map(UserDto::getId).collect(Collectors.toSet()));
        return taskFilterDto;
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("id")) {
            id.setValue(Integer.valueOf(map.get("id")));
        }
        if (map.containsKey("owner")) {
            UserDto userDto = userDtoList.stream()
                    .filter(actualUserDto -> actualUserDto.getId() == Long.valueOf(map.get("owner")))
                    .findFirst().orElse(null);
            owner.setValue(userDto);
        }
        if (map.containsKey("name")) {
            search.setValue(map.get("name"));
        }
        if (map.containsKey("showOnlyMine")) {
            showMeSolver.setValue(Boolean.valueOf(map.get("showOnlyMine")));
        }
        /*
        if (map.containsKey("dateFrom")) {
            dateFrom.setValue(LocalDate.parse(map.get("dateFrom")));
        }
        if (map.containsKey("dateTo")) {
            dateTo.setValue(LocalDate.parse(map.get("dateTo")));
        }
         */

    }

    public void fillUrl() {
        String paramUrl = TaskView.ROUTE.concat("/");
        if (id.getValue() != null) {
            paramUrl = paramUrl.concat("&id=").concat(String.valueOf(id.getValue()));
        }
        if (owner.getValue() != null) {
            paramUrl = paramUrl.concat("&owner=").concat(String.valueOf(owner.getValue().getId()));
        }
        if (!search.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&name=").concat(search.getValue());
        }
        /*
        if (dateFrom.getValue() != null) {
            paramUrl = paramUrl.concat("&dateFrom=").concat(dateFrom.getValue().toString());
        }
        if (dateTo.getValue() != null) {
            paramUrl = paramUrl.concat("&dateTo=").concat(dateTo.getValue().toString());
        }
         */
        paramUrl = paramUrl.concat("&showOnlyMine=").concat(showMeSolver.getValue().toString());

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    private String getTranslatedObjectType(ObjectType objectTypeTemp) {
        return Transl.get(objectTypeTemp.name());
    }
}
