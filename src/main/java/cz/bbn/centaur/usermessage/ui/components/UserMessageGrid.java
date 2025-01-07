package cz.bbn.cerberus.usermessage.ui.components;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.listbox.ListBox;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.AppHelp;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.GetItemAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.usermessage.UserMessageUtils;
import cz.bbn.cerberus.usermessage.dto.UserMessageDto;
import org.apache.commons.lang3.StringUtils;

public class UserMessageGrid extends AppInfiniteGrid<UserMessageDto> {

    private final SaveAction<UserMessageDto> saveAction;
    private final UserMessageDialog messageDialog;
    private final GetItemAction<UserMessageDto> getItemAction;

    private UserMessageDto lastUnviewedUserMessageDto;

    protected UserMessageGrid(AppEnv appEnv, ItemsAction<UserMessageDto> itemsAction,
                              SaveAction<UserMessageDto> saveAction, UserMessageDialog messageDialog,
                              GetItemAction<UserMessageDto> getItemAction) {
        super(appEnv, itemsAction);
        this.saveAction = saveAction;
        this.messageDialog = messageDialog;
        this.getItemAction = getItemAction;
        initGrid();
    }

    private void initGrid() {
        this.setClassName("table-user-message");
        addColumn(new ComponentRenderer<>(this::getGridColumn));
    }

    private VerticalLayout getGridColumn(UserMessageDto clickedItem) {
        VerticalLayout content = new VerticalLayout();
        content.setClassName("user-message-content");

        HorizontalLayout header = new HorizontalLayout();
        header.getElement().getStyle().set("background", "hsl("
                + UserMessageUtils.getHeaderStyle(clickedItem.getPriority(), getAppEnv())
                + ", 63%, 83%)");
        header.setAlignItems(FlexComponent.Alignment.BASELINE);
        header.setWidthFull();
        header.setClassName("user-message-header");
        H4 title = new H4(Transl.get(clickedItem.getType().name()));
        title.setWidth("60%");

        HorizontalLayout infoLayout = new HorizontalLayout();
        infoLayout.setAlignItems(FlexComponent.Alignment.STRETCH);
        AppHelp info = new AppHelp(header, null);
        info.setContent(getListBox(clickedItem));
        infoLayout.setWidth("5%");
        infoLayout.add(info);

        Checkbox viewed = new Checkbox(Transl.get("Viewed"));
        viewed.setClassName("message-viewed-checkbox");
        if (Boolean.FALSE.equals(clickedItem.getViewed())) {
            viewed.addValueChangeListener(event -> {
                saveAction.saveItem(clickedItem, null);
                viewed.setReadOnly(true);
                loadData();
            });
        } else {
            viewed.setReadOnly(true);
            viewed.setValue(true);
        }
        viewed.setWidth("20%");

        header.add(infoLayout, title, viewed);

        HorizontalLayout body = new HorizontalLayout();
        body.setClassName("user-message-body");
        body.setWidthFull();
        body.getElement().getStyle().set(UserMessageUtils.BACKGROUND_COLOR, "hsl("
                + UserMessageUtils.getBodyStyle(clickedItem.getDueDate().toLocalDate(), getAppEnv())
                + UserMessageUtils.HSL);

        Div message = new Div();
        message.getElement().setProperty("innerHTML", clickedItem.getMessage());
        message.addClassName("user-message-div");
        message.setWidthFull();
        message.setHeight("4em");

        body.add(message);

        if (StringUtils.isNoneEmpty(clickedItem.getObjectId())) {
            Button eye = VaadinComponents.getViewButton();
            eye.addClickListener(buttonClickEvent -> {
                UI.getCurrent().navigate(
                        UserMessageUtils.getUserMessageLink(clickedItem.getObjectType(), clickedItem.getObjectId()));
                messageDialog.close();
            });
            body.add(eye);
        }

        content.add(header);
        content.add(body);

        if (lastUnviewedUserMessageDto != null && lastUnviewedUserMessageDto.getId().equals(clickedItem.getId())) {
            Span separator = new Span();
            separator.setClassName("separator");
            VerticalLayout verticalLayout = new VerticalLayout(content, separator);
            verticalLayout.setMargin(false);
            verticalLayout.setPadding(false);
            return verticalLayout;
        } else {
            return content;
        }
    }

    private ListBox<String> getListBox(UserMessageDto dto) {
        ListBox<String> listBox = new ListBox<>();
        listBox.setItems(
                Transl.get("Is priority message").concat(": ")
                        .concat(dto.getPriority() ? Transl.get("Yes") : Transl.get("No")),
                Transl.get("Due date").concat(": ").concat(AppUtils.formatDateTime(dto.getDueDate(), true)),
                Transl.get("Remaining days")
                        .concat(": ")
                        .concat(Transl.get(
                                UserMessageUtils.getDaysRemaining(dto.getDueDate().toLocalDate(), getAppEnv()))),
                Transl.get("Object type")
                        .concat(": ")
                        .concat(dto.getObjectType() != null ? Transl.get(dto.getObjectType().name()) : "")
                        .concat(", ")
                        .concat(Transl.get("Object id"))
                        .concat(": ")
                        .concat(!StringUtils.isEmpty(dto.getObjectId()) ? dto.getObjectId() : ""));
        return listBox;
    }

    @Override
    public void loadData() {
        super.loadData();
        lastUnviewedUserMessageDto = getItemAction.getItem(null);
    }
}
