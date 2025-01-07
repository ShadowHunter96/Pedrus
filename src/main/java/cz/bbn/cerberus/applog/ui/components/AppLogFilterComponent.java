package cz.bbn.cerberus.applog.ui.components;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.applog.dto.AppLogFilterDto;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.ArrayList;
import java.util.List;

public class AppLogFilterComponent extends FormLayout {

    private final Button searchButton;
    private final ListAction<ItemDto> listActionActionList;
    private final UserService userService;

    private ComboBox<ItemDto> action;
    private ComboBox<UserDto> user;
    private TextField message;
    private TextField appId;

    public AppLogFilterComponent(Button searchButton, ListAction<ItemDto> listActionActionList,
                                 UserService userService) {
        this.searchButton = searchButton;
        this.listActionActionList = listActionActionList;
        this.userService = userService;
        initComponent();
    }

    private void initComponent() {
        action = new ComboBox<>(Transl.get("Action"));
        action.setItems(getActionList());
        action.setValue(new ItemDto("", Transl.get("All")));
        action.setItemLabelGenerator(ItemDto::getName);
        this.add(action);

        user = new ComboBox<>(Transl.get("User"));
        user.setItems(userService.findUserList());
        user.setValue(new UserDto(Transl.get("All")));
        user.setItemLabelGenerator(UserDto::getName);
        this.add(user);

        message = new TextField(Transl.get("Message"));
        this.add(message);

        appId = new TextField(Transl.get("Id"));
        this.add(appId);

        this.add(searchButton);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    private List<ItemDto> getActionList() {
        List<ItemDto> list = new ArrayList<>();
        list.add(new ItemDto("", Transl.get("All")));
        list.addAll(listActionActionList.getList(null));
        return list;
    }

    public AppLogFilterDto getAppLogFilterDto() {
        AppLogFilterDto appLogFilter = new AppLogFilterDto();
        appLogFilter.setAction(action.getValue().getId());
        appLogFilter.setUserId(user.getValue().getId());
        appLogFilter.setMessage(message.getValue());
        appLogFilter.setAppId(appId.getValue());
        return appLogFilter;
    }

}
