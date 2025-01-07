package cz.bbn.cerberus.usermessage.ui.components;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.listbox.ListBox;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import cz.bbn.cerberus.commons.component.ui.AppHelp;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.GetItemAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.usermessage.UserMessageObjectType;
import cz.bbn.cerberus.usermessage.UserMessageService;
import cz.bbn.cerberus.usermessage.UserMessageUtils;
import cz.bbn.cerberus.usermessage.dto.UserMessageDto;
import cz.bbn.cerberus.usermessage.dto.UserMessageFilterDto;


public class UserMessageDialog extends AppDialog {

    private static final String HEIGHT = "1em";
    private static final String WIDTH = "5em";
    private static final String REMAINING_DAYS = "Remaining number of days less than";

    private final UserMessageService userMessageService;
    private final AppEnv appEnv;
    private final MainLayout mainLayout;

    public UserMessageDialog(UserMessageService userMessageService, AppEnv appEnv, MainLayout mainLayout) {
        this.userMessageService = userMessageService;
        this.appEnv = appEnv;
        this.mainLayout = mainLayout;
        initComponent();
    }

    private void initComponent() {
        removeAll();

        this.setHeight("90%");
        setTitle(Transl.get("User message"));
        HorizontalLayout filterLayout = new HorizontalLayout();
        Checkbox orderByPriority = new Checkbox(Transl.get("Order by priority"));
        ComboBox<UserMessageObjectType> type = new ComboBox<>(Transl.get("Filter by type"));
        type.setItems(UserMessageObjectType.values());
        type.setItemLabelGenerator(userMessageObjectType -> Transl.get(userMessageObjectType.name()));
        type.setValue(UserMessageObjectType.ALL);

        AppHelp info = new AppHelp(this.getContent(), Transl.get("Color legend").concat(":"));
        info.setContent(getListBox());
        info.getAppTooltip().getElement().getStyle().set("position", "fixed");
        info.getAppTooltip().getElement().getStyle().set("top", "15em");

        filterLayout.setAlignItems(FlexComponent.Alignment.BASELINE);
        filterLayout.add(orderByPriority, type, info);
        UserMessageGrid userMessageGrid = new UserMessageGrid(appEnv, getItemAction(orderByPriority, type),
                getSaveAction(mainLayout), this, getUserMessageDtoGetItemAction(orderByPriority, type));
        userMessageGrid.loadData();
        setContent(filterLayout, userMessageGrid);

        orderByPriority.addValueChangeListener(checkboxBooleanComponentValueChangeEvent -> userMessageGrid.loadData());

        type.addValueChangeListener(
                comboBoxUserMessageObjectTypeComponentValueChangeEvent -> userMessageGrid.loadData());

        Button close = VaadinComponents.getCloseButton();
        close.addClickListener(buttonClickEvent -> this.close());
        addButtons(close);
    }

    private GetItemAction<UserMessageDto> getUserMessageDtoGetItemAction(Checkbox orderByPriority,
                                                                         ComboBox<UserMessageObjectType> type) {
        return id -> {
            UserMessageFilterDto filterDto = new UserMessageFilterDto();
            filterDto.setUserId(SecurityUtils.getCurrentUserId());
            filterDto.setObjectType(type.getValue());
            filterDto.setOrderByPriority(orderByPriority.getValue());
            return userMessageService.findLastUnViewed(filterDto);
        };
    }

    private ItemsAction<UserMessageDto> getItemAction(Checkbox orderByPriority, ComboBox<UserMessageObjectType> type) {
        return (query, orderList) -> {
            UserMessageFilterDto filterDto = new UserMessageFilterDto();
            filterDto.setUserId(SecurityUtils.getCurrentUserId());
            filterDto.setObjectType(type.getValue());
            filterDto.setOrderByPriority(orderByPriority.getValue());
            filterDto.setPage(query.getOffset());
            filterDto.setSize(query.getPageSize());
            return userMessageService.findUserMessageDtoPage(filterDto);
        };
    }

    private SaveAction<UserMessageDto> getSaveAction(MainLayout mainLayout) {
        return (dto, originalDto) -> {
            userMessageService.saveViewed(dto.getId());
            mainLayout.refreshMessageCount();
            SuccessNotification.show(Transl.get("Save successfull"), appEnv);
        };
    }

    private ListBox<Component> getListBox() {
        ListBox<Component> listBox = new ListBox<>();

        HorizontalLayout priorityColor = new HorizontalLayout();
        priorityColor.setHeight(HEIGHT);
        priorityColor.setWidth(WIDTH);
        priorityColor.getElement().getStyle().set(UserMessageUtils.BACKGROUND_COLOR, "hsl("
                + appEnv.getUserMessageColorPriority()
                + UserMessageUtils.HSL);
        Label priorityLabel = new Label(" - ".concat(Transl.get("High priority color")));
        priorityLabel.setWidth("10em");

        HorizontalLayout normalPriorityColor = new HorizontalLayout();
        normalPriorityColor.setHeight(HEIGHT);
        normalPriorityColor.setWidth(WIDTH);
        normalPriorityColor.getElement().getStyle().set(UserMessageUtils.BACKGROUND_COLOR, "hsl("
                + appEnv.getUserMessageColorNormal()
                + UserMessageUtils.HSL);
        Label normalPriorityLabel = new Label(" - ".concat(Transl.get("Normal priority color")));

        HorizontalLayout level1Collor = new HorizontalLayout();
        level1Collor.setHeight(HEIGHT);
        level1Collor.setWidth(WIDTH);
        level1Collor.getElement().getStyle().set(UserMessageUtils.BACKGROUND_COLOR, "hsl("
                + appEnv.getUserMessageColorLevel1()
                + UserMessageUtils.HSL);
        Label level1Label = new Label(" - ".concat(Transl.get(REMAINING_DAYS).concat(" - ")
                .concat(String.valueOf(appEnv.getUserMessageDaysLevel1()))));

        HorizontalLayout level2Collor = new HorizontalLayout();
        level2Collor.setHeight(HEIGHT);
        level2Collor.setWidth(WIDTH);
        level2Collor.getElement().getStyle().set(UserMessageUtils.BACKGROUND_COLOR, "hsl("
                + appEnv.getUserMessageColorLevel2()
                + UserMessageUtils.HSL);
        Label level2Label = new Label(" - ".concat(Transl.get(REMAINING_DAYS).concat(" - ")
                .concat(String.valueOf(appEnv.getUserMessageDaysLevel2()))));

        HorizontalLayout level3Collor = new HorizontalLayout();
        level3Collor.setHeight(HEIGHT);
        level3Collor.setWidth(WIDTH);
        level3Collor.getElement().getStyle().set(UserMessageUtils.BACKGROUND_COLOR, "hsl("
                + appEnv.getUserMessageColorLevel3()
                + UserMessageUtils.HSL);
        Label level3Label = new Label(" - ".concat(Transl.get(REMAINING_DAYS).concat(" - ")
                .concat(String.valueOf(appEnv.getUserMessageDaysLevel3()))));

        Label chillLabelCollor = new Label(Transl.get("white"));
        chillLabelCollor.setHeight(HEIGHT);
        chillLabelCollor.setWidth(WIDTH);

        Label chillLabel = new Label(" - ".concat(Transl.get("Chill chill")));

        listBox.add(new HorizontalLayout(priorityColor, priorityLabel),
                new HorizontalLayout(normalPriorityColor, normalPriorityLabel),
                new HorizontalLayout(level1Collor, level1Label),
                new HorizontalLayout(level2Collor, level2Label),
                new HorizontalLayout(level3Collor, level3Label),
                new HorizontalLayout(chillLabelCollor, chillLabel));
        return listBox;
    }
}
