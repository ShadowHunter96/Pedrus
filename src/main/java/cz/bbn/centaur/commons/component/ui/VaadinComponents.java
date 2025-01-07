package cz.bbn.cerberus.commons.component.ui;

import com.google.common.collect.Lists;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.datetimepicker.DateTimePicker;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppSlideTab;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.enums.FilterBoolean;
import cz.bbn.cerberus.commons.component.ui.slidetab.CountIntIndicator;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.note.ui.component.NoteComponent;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.task.ui.component.TaskSlideTabComponent;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;
import org.vaadin.addons.badge.Badge;
import org.vaadin.erik.SlideMode;
import org.vaadin.erik.SlideTab;
import org.vaadin.erik.SlideTabBuilder;
import org.vaadin.erik.SlideTabPosition;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Locale;


/**
 * Trida pro vytvareni specialnich vaadinovskych component
 */
public class VaadinComponents {

    public static final int DESCRIPTION_MAX_LENGTH = 255;

    private VaadinComponents() {
    }

    public static Span headerSpan(String text) {
        return new Span(text);
    }

    public static Span spanBold(String text) {
        Span spanBold = new Span();
        spanBold.setText(text);
        spanBold.getElement().getStyle().set("font-weight", "bold");
        return spanBold;
    }

    public static Label titleLabelSmall(String text) {
        Label titleSmallLabel = new Label(text);
        titleSmallLabel.getElement().getClassList().add("title-label-small");
        return titleSmallLabel;
    }

    public Label withWhiteSpaceNoWrapSmall(String text) {
        Label label = titleLabelSmall(text);
        label.getElement().getStyle().set("white-space", "nowrap");
        return label;
    }

    public static Label titleLabelBig(String text) {
        Label titleSmallLabel = new Label(text);
        titleSmallLabel.getElement().getClassList().add("title-label-big");
        titleSmallLabel.getElement().getStyle().set("color", CssVariables.LUMO_PRIMARY_TEXT_COLOR.getValue());
        return titleSmallLabel;
    }

    public Label withWhiteSpaceNoWrapBig(String text) {
        Label label = titleLabelBig(text);
        label.getElement().getStyle().set("white-space", "nowrap");
        return label;
    }

    public static DateTimePicker getDateTimePicker(String title,
                                                   LocalDateTime localDateTime) {
        DateTimePicker dateTimePicker = new AppDateTimePicker(title, localDateTime);
        dateTimePicker.setLocale(Locale.forLanguageTag(Transl.DEFAULT_LANG.toLowerCase()));

        DatePicker.DatePickerI18n datePickerI18n = new DatePicker.DatePickerI18n();
        datePickerI18n.setDateFormats("dd.MM.yyyy", "dd-MM-yyyy");
        datePickerI18n.setCancel(Transl.get("Cancel"));
        datePickerI18n.setWeek(Transl.get("Week"));
        datePickerI18n.setToday(Transl.get("Today"));
        datePickerI18n.setWeekdaysShort(Lists.newArrayList(Transl.get("Sun"), Transl.get("Mon"),
                Transl.get("Tue"), Transl.get("Wed"), Transl.get("Thu"), Transl.get("Fri"), Transl.get("Sat")));

        datePickerI18n.setMonthNames(Lists.newArrayList(Transl.get("January"), Transl.get("February"),
                Transl.get("March"), Transl.get("April"), Transl.get("May"), Transl.get("June"), Transl.get("July"),
                Transl.get("August"), Transl.get("September"), Transl.get("October"), Transl.get("November"),
                Transl.get("December")));

        datePickerI18n.setFirstDayOfWeek(1);
        dateTimePicker.setDatePickerI18n(datePickerI18n);
        return dateTimePicker;
    }

    public static DatePicker getDatePicker(LocalDate localDateTime) {
        return getDatePicker(null, localDateTime);
    }

    public static DatePicker getDatePicker(String title, LocalDate localDateTime) {
        DatePicker datePicker;
        if (title == null) {
            datePicker = new AppDatePicker(localDateTime);
        } else {
            datePicker = new AppDatePicker(title, localDateTime);
        }
        datePicker.setLocale(Locale.forLanguageTag(Transl.DEFAULT_LANG.toLowerCase()));

        DatePicker.DatePickerI18n datePickerI18n = new DatePicker.DatePickerI18n();
        datePickerI18n.setDateFormats("dd.MM.yyyy", "dd-MM-yyyy", "dd.MM yyyy",
                "dd.MM. yyyy");
        datePickerI18n.setCancel(Transl.get("Cancel"));
        datePickerI18n.setWeek(Transl.get("Week"));
        datePickerI18n.setToday(Transl.get("Today"));
        datePickerI18n.setWeekdaysShort(Lists.newArrayList(Transl.get("Sun"), Transl.get("Mon"),
                Transl.get("Tue"), Transl.get("Wed"), Transl.get("Thu"), Transl.get("Fri"), Transl.get("Sat")));

        datePickerI18n.setMonthNames(Lists.newArrayList(Transl.get("January"), Transl.get("February"),
                Transl.get("March"), Transl.get("April"), Transl.get("May"), Transl.get("June"), Transl.get("July"),
                Transl.get("August"), Transl.get("September"), Transl.get("October"), Transl.get("November"),
                Transl.get("December")));

        datePickerI18n.setFirstDayOfWeek(1);
        datePicker.setI18n(datePickerI18n);
        datePicker.addOpenedChangeListener(e -> {
            if (datePicker.getValue() == null) {
                datePicker.getElement().executeJs("this.inputElement.value = ''");
            }
        });
        return datePicker;
    }

    public static Button getNewButton(String text) {
        return getNewButton(text, true);
    }

    public static Button getNewButton(String text, boolean primary) {
        Button button = new Button(text, getPlusIcon());
        if (primary) {
            button.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        }
        button.addClassName(RobotFrameworkVariables.ADD_ITEM_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, text);
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());

        return button;
    }

    public static Button getLinkButton(String text) {
        Button button = new Button(text, VaadinIcon.LINK.create());
        button.addClassName(RobotFrameworkVariables.ADD_ITEM_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, text);
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getSubmitButton() {
        Button button = new Button(Transl.get("Submit"), VaadinIcon.CLOUD_UPLOAD.create());
        button.addClassName(RobotFrameworkVariables.SUBMIT_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, Transl.get("Submit"));
        button.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getConfirmButton() {
        Button button = new Button(Transl.get("Confirm"), VaadinIcon.CHECK.create());
        button.addClassName(RobotFrameworkVariables.SUBMIT_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, Transl.get("Confirm"));
        button.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getBackButton() {
        Button button = new Button(Transl.get("Back"), VaadinIcon.ARROW_BACKWARD.create());
        button.addClassName(RobotFrameworkVariables.BACK_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, Transl.get("Back"));
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getCloseButton() {
        Button button = new Button(Transl.get("Close"), VaadinIcon.CLOSE.create());
        button.addClassName(RobotFrameworkVariables.CANCEL_DIALOG_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, Transl.get("Close"));
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getDeleteButton() {
        Button button = new Button(VaadinIcon.TRASH.create());
        button.addClassName(RobotFrameworkVariables.DELETE_ITEM_BUTTON_CLASS.getValue());
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getUnlinkButton() {
        Button button = new Button(VaadinIcon.UNLINK.create());
        button.addClassName(RobotFrameworkVariables.UNLINK_ITEM_BUTTON_CLASS.getValue());
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getDeleteButton(boolean recoveryButton) {
        Button button = !recoveryButton ? new Button(VaadinIcon.TRASH.create()) : new Button(VaadinIcon.SHARE.create());
        button.addClassName(RobotFrameworkVariables.DELETE_ITEM_BUTTON_CLASS.getValue());
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getEditButton() {
        Button button = new Button(VaadinIcon.EDIT.create());
        button.addClassName(RobotFrameworkVariables.EDIT_ITEM_BUTTON_CLASS.getValue());
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getSearchButton() {
        Button button = new Button(Transl.get("Reload"), VaadinIcon.REFRESH.create());
        button.addClassName(RobotFrameworkVariables.SEARCH_BUTTON_CLASS.getValue());
        button.addClickShortcut(Key.ENTER);
        button.getElement().setProperty(TextValues.TITLE, Transl.get("Reload"));
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getClearFilterButton() {
        Button button = new Button(Transl.get("Clear"), VaadinIcon.ROTATE_LEFT.create());
        button.addClassName(RobotFrameworkVariables.CLEAR_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, Transl.get("Clear"));
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getViewButton() {
        Button button = new Button(VaadinIcon.EYE.create());
        button.addClassName(RobotFrameworkVariables.DETAIL_ITEM_BUTTON_CLASS.getValue());
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getEyeButton() {
        Button button = new Button(VaadinIcon.EYE.create());
        button.addClassName(RobotFrameworkVariables.EYE_BUTTON_CLASS.getValue());
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        button.addClassName("eye-button");
        return button;
    }

    public static Button getChangeButton(String changeText) {
        Button button = new Button(changeText, VaadinIcon.EXCHANGE.create());
        button.addClassName(RobotFrameworkVariables.CHANGE_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, changeText);
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getShowFilterButton() {
        return getShowFilterButton("Show filter");
    }

    public static Button getShowFilterButton(String text) {
        Button button = new Button(Transl.get(text), VaadinIcon.CHEVRON_DOWN_SMALL.create());
        button.addClassName(RobotFrameworkVariables.SHOW_FILTER_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, Transl.get(text));
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getHideFilterButton() {
        return getHideFilterButton("Hide filter");
    }

    public static Button getHideFilterButton(String text) {
        Button button = new Button(Transl.get(text), VaadinIcon.CHEVRON_UP_SMALL.create());
        button.addClassName(RobotFrameworkVariables.HIDE_FILTER_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, Transl.get(text));
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getLinkMultipleButton() {
        Button button = new Button(Transl.get("Link"), VaadinIcon.PLUS.create());
        button.addClassName(RobotFrameworkVariables.LINK_MULTIPLE_BUTTON_CLASS.getValue());
        button.getElement().setProperty(TextValues.TITLE, Transl.get("Link"));
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getButton(String text) {
        Button button = new Button(text);
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        button.getElement().setProperty(TextValues.TITLE, text);
        return button;
    }

    public static Button getButton(Icon icon) {
        Button button = new Button(icon);
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        return button;
    }

    public static Button getButton(String text, Icon icon) {
        Button button = new Button(text, icon);
        button.addClassName(CssVariables.CURSOR_POINTER.getValue());
        button.getElement().setProperty(TextValues.TITLE, text);
        return button;
    }

    public static Icon getInfoIcon() {
        return VaadinIcon.INFO_CIRCLE.create();
    }

    public static Anchor getSubjectLink(SubjectDto subjectDto) {
        H3 h3 = new H3(Transl.get("Subject").concat(": ").concat(subjectDto.getName()));
        Anchor link = new Anchor(SubjectDetailView.ROUTE.concat("/").concat(subjectDto.getId()), h3);
        link.setClassName("subject-link");
        return link;
    }

    public static Anchor getSubjectLinkDialog(SubjectDto subjectDto, AppDialog dialog) {
        H3 h3 = new H3(Transl.get("Subject").concat(": ").concat(subjectDto.getName()));
        Anchor link = new Anchor(SubjectDetailView.ROUTE.concat("/").concat(subjectDto.getId()), h3);
        link.getElement().addEventListener("click", event -> dialog.close());
        link.setClassName("subject-link-dialog");
        return link;
    }

    public static Icon getBooleanIcon(boolean booleanValue) {
        if (booleanValue) {
            Icon icon = new Icon(VaadinIcon.CHECK);
            icon.setColor(CssVariables.LUMO_SUCCESS_COLOR.getValue());
            return icon;
        }
        Icon icon = new Icon(VaadinIcon.CLOSE);
        icon.setColor(CssVariables.LUMO_ERROR_COLOR.getValue());
        return icon;
    }

    public static SlideTab createNoteSlideTab(NoteComponent noteComponent, int position) {
        SlideTabBuilder slideTabBuilder = new SlideTabBuilder(noteComponent);
        slideTabBuilder.tabPosition(SlideTabPosition.MIDDLE);
        slideTabBuilder.mode(SlideMode.TOP);

        SlideTab slideTab = new AppSlideTab(slideTabBuilder, "Notes", SlideMode.TOP);
        if (position == 0) {
            slideTab.getElement().getStyle().set("left", "calc(50% - 19em)");
        }
        if (position == 1) {
            slideTab.getElement().getStyle().set("left", "calc(50% - 9em)");
        }
        if (position == 2) {
            slideTab.getElement().getStyle().set("left", "calc(50% + 1em)");
        }
        slideTab.setClassName("slide-tab-notes", true);
        return slideTab;
    }

    public static SlideTab createEventSlideTab(TaskSlideTabComponent taskSlideTabComponent,
                                               int position, CountIntIndicator countIntIndicator) {
        SlideTabBuilder slideTabBuilder = new SlideTabBuilder(taskSlideTabComponent);
        slideTabBuilder.tabPosition(SlideTabPosition.MIDDLE);
        slideTabBuilder.mode(SlideMode.TOP);

        AppSlideTab slideTab = new AppSlideTab(slideTabBuilder, "Events", SlideMode.TOP, countIntIndicator);
        slideTab.setClassName("slide-tab-events", true);
        if (position == 0) {
            slideTab.getElement().getStyle().set("left", "calc(50% - 30em)");
        }
        if (position == 1) {
            slideTab.getElement().getStyle().set("left", "calc(50% - 22.0em)");
        }
        if (position == 2) {
            slideTab.getElement().getStyle().set("left", "calc(50% - 11.5em)");
        }
        taskSlideTabComponent.setAppSlideTab(slideTab);
        return slideTab;
    }

    public static SlideTab createNewEntityButtonsSlideTab(NewEntityButtonsComponent newEntityButtonsComponent,
                                                          int position) {
        SlideTabBuilder slideTabBuilder = new SlideTabBuilder(newEntityButtonsComponent);
        slideTabBuilder.tabPosition(SlideTabPosition.MIDDLE);
        slideTabBuilder.mode(SlideMode.TOP);

        AppSlideTab slideTab = new AppSlideTab(slideTabBuilder, "New entity", SlideMode.TOP);
        if (position == 0) {
            slideTab.getElement().getStyle().set("left", "calc(50% - 30em)");
        }
        if (position == 1) {
            slideTab.getElement().getStyle().set("left", "calc(50% - 22.0em)");
        }
        if (position == 2) {
            slideTab.getElement().getStyle().set("left", "calc(50% - 14.0em)");
        }
        slideTab.setClassName("slide-tab-buttons", true);
        return slideTab;
    }

    public static HorizontalLayout getCheckUncheckLayoutNullTrue(Boolean allowed) {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        Badge badge = new Badge();
        if (Boolean.FALSE.equals(allowed)) {
            badge.setIcon(VaadinIcon.CLOSE.create());
            badge.setVariant(Badge.BadgeVariant.ERROR);
        } else {
            badge.setIcon(VaadinIcon.CHECK.create());
            badge.setVariant(Badge.BadgeVariant.SUCCESS);
        }
        horizontalLayout.setClassName("center-icon");
        horizontalLayout.add(badge);
        return horizontalLayout;
    }

    public static HorizontalLayout getCheckUncheckLayoutNullFalse(Boolean value) {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        Badge badge = new Badge();
        if (Boolean.TRUE.equals(value)) {
            badge.setIcon(VaadinIcon.CHECK.create());
            badge.setVariant(Badge.BadgeVariant.SUCCESS);
        } else {
            badge.setIcon(VaadinIcon.CLOSE.create());
            badge.setVariant(Badge.BadgeVariant.ERROR);
        }
        horizontalLayout.setClassName("center-icon");
        horizontalLayout.add(badge);
        return horizontalLayout;
    }

    public static ComboBox<VaadinIcon> getVaadinIconComboBox() {
        ComboBox<VaadinIcon> vaadinIcons = new ComboBox<>(Transl.get("Icon"));
        vaadinIcons.setItems(VaadinIcon.values());
        vaadinIcons.setRenderer(new ComponentRenderer<>(VaadinComponents::getIconRenderer));
        vaadinIcons.setMinWidth("20em");
        return vaadinIcons;
    }

    public static ComboBox<Badge.BadgeVariant> getBadgeVariantComboBox() {
        ComboBox<Badge.BadgeVariant> badgeVariant = new ComboBox<>(Transl.get("Color variant"));
        badgeVariant.setItems(Badge.BadgeVariant.values());
        badgeVariant.setItemLabelGenerator(
                badgeColorVariantEnum -> Transl.get(badgeColorVariantEnum.name().toLowerCase()));
        return badgeVariant;
    }

    public static Icon getPlusIcon() {
        Icon plusIcon = VaadinIcon.PLUS_CIRCLE_O.create();
        plusIcon.getElement().getStyle().set("padding-right", "8px");
        return plusIcon;
    }

    public static HorizontalLayout getPctColumn(Integer pct) {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        Label pctLabel = new Label(pct.toString().concat(" %"));
        pctLabel.setClassName("number-column");
        horizontalLayout.setWidthFull();
        horizontalLayout.add(pctLabel);
        return horizontalLayout;
    }

    public static HorizontalLayout getCenteredNumberColumn(Integer number) {
        return getCenteredNumberColumn(number == null ? 0 : Double.valueOf(number));
    }

    public static HorizontalLayout getCenteredNumberColumn(Double number) {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.setClassName("buttons-layout");
        Label label = new Label(number == null ? "0" : (number % 1) == 0 ? String.valueOf(number.intValue()) : number.toString());
        label.setClassName("centered-label");
        horizontalLayout.setWidthFull();
        horizontalLayout.add(label);
        return horizontalLayout;
    }

    public static HorizontalLayout getCenteredColumn(String value) {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.setClassName("buttons-layout");
        Label label = new Label(value);
        label.setClassName("centered-label");
        horizontalLayout.setWidthFull();
        horizontalLayout.add(label);
        return horizontalLayout;
    }

    public static void generateBadgeViewLayout(HorizontalLayout layout, String name, Badge.BadgeVariant badgeVariant,
                                               VaadinIcon vaadinIcon) {
        layout.removeAll();
        layout.setAlignItems(FlexComponent.Alignment.CENTER);
        if (StringUtils.isNoneEmpty(name) && badgeVariant != null && vaadinIcon != null) {
            layout.add(new Label(Transl.get("View").concat(": ")), VaadinComponents.getBadge(
                    name,
                    badgeVariant,
                    vaadinIcon));
        }
    }

    public static Badge getBadge(String name, Badge.BadgeVariant badgeVariant, VaadinIcon vaadinIcon) {
        Badge badge = new Badge(name);
        badge.setVariant(badgeVariant);
        Icon icon = vaadinIcon.create();
        icon.setClassName("badge-icon");
        badge.setIcon(icon);
        return badge;
    }

    public static ComboBox<FilterBoolean> getFilterBooleanComboBox(String title){
        ComboBox<FilterBoolean> comboBox = new ComboBox<>(title);
        comboBox.setItems(FilterBoolean.values());
        comboBox.setItemLabelGenerator(filterBoolean -> Transl.get(filterBoolean.getTitle()));
        comboBox.setValue(FilterBoolean.ALL);
        return comboBox;
    }

    private static HorizontalLayout getIconRenderer(VaadinIcon item) {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.add(item.create());
        horizontalLayout.add(new Label(item.name()));
        return horizontalLayout;
    }

}
