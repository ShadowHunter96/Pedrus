package cz.bbn.cerberus.azure;

import com.google.gson.JsonPrimitive;
import com.microsoft.bot.restclient.interceptors.CustomHeadersInterceptor;
import com.microsoft.graph.models.AadUserConversationMember;
import com.microsoft.graph.models.BodyType;
import com.microsoft.graph.models.Chat;
import com.microsoft.graph.models.ChatMessage;
import com.microsoft.graph.models.ChatType;
import com.microsoft.graph.models.ConversationMember;
import com.microsoft.graph.models.DateTimeTimeZone;
import com.microsoft.graph.models.Event;
import com.microsoft.graph.models.ItemBody;
import com.microsoft.graph.options.HeaderOption;
import com.microsoft.graph.options.Option;
import com.microsoft.graph.requests.ConversationMemberCollectionPage;
import com.microsoft.graph.requests.ConversationMemberCollectionResponse;
import com.microsoft.graph.requests.GraphServiceClient;
import cz.bbn.cerberus.azure.dto.OutlookEventDto;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.task.dto.TaskDto;
import okhttp3.OkHttpClient;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;

@Service
public class AzureGraphService {

    public void createChat(String targetUser, String message) {
        Chat chat = new Chat();
        chat.chatType = ChatType.ONE_ON_ONE;
        LinkedList<ConversationMember> membersList = new LinkedList<>();
        AadUserConversationMember members = new AadUserConversationMember();
        LinkedList<String> rolesList = new LinkedList<>();
        rolesList.add("owner");
        members.roles = rolesList;
        members.additionalDataManager().put(
                "user@odata.bind",
                new JsonPrimitive("https://graph.microsoft.com/v1.0/users('" + SecurityUtils.getLoginName() + "')"));
        members.additionalDataManager().put(
                "@odata.type", new JsonPrimitive("#microsoft.graph.aadUserConversationMember"));
        membersList.add(members);
        AadUserConversationMember members1 = new AadUserConversationMember();
        LinkedList<String> rolesList1 = new LinkedList<>();
        rolesList1.add("owner");
        members1.roles = rolesList1;
        members1.additionalDataManager().put(
                "user@odata.bind", new JsonPrimitive("https://graph.microsoft.com/v1.0/users('" + targetUser + "')"));
        members1.additionalDataManager().put(
                "@odata.type", new JsonPrimitive("#microsoft.graph.aadUserConversationMember"));
        membersList.add(members1);
        ConversationMemberCollectionResponse conversationMemberCollectionResponse =
                new ConversationMemberCollectionResponse();
        conversationMemberCollectionResponse.value = membersList;
        ConversationMemberCollectionPage conversationMemberCollectionPage =
                new ConversationMemberCollectionPage(conversationMemberCollectionResponse, null);
        chat.members = conversationMemberCollectionPage;

        Chat actualChat = getGraphServiceClientDelegatedUser().chats()
                .buildRequest()
                .post(chat);

        ChatMessage chatMessage = new ChatMessage();
        ItemBody body = new ItemBody();
        body.content = message;
        chatMessage.body = body;

        if (actualChat.id != null) {
            getGraphServiceClientDelegatedUser().chats(actualChat.id).messages()
                    .buildRequest()
                    .post(chatMessage);
        }
    }

    public List<OutlookEventDto> getOutlookEventDtoList(LocalDateTime start, LocalDateTime end) {
        List<OutlookEventDto> outlookEventDtoList = new ArrayList<>();

        // @TODO funkcionalita pada po 60 minutach. Zrejme je casove omezeny token a neprenacte se. Vyresit az se bude pracovat na outlookovem ukolu
       /* EventCollectionPage eventCollectionPage = getGraphServiceClientDelegatedUser()
                .me()
                .calendar()
                .events()
                .buildRequest()
                .get();
        eventCollectionPage.getCurrentPage().forEach(event -> {
            OutlookEventDto dto = new OutlookEventDto();
            dto.setSubject(event.subject);
            dto.setFrom(LocalDateTime.parse(event.start.dateTime));
            dto.setTo(LocalDateTime.parse(event.end.dateTime));
            outlookEventDtoList.add(dto);
        });*/
        return outlookEventDtoList;
    }

    public void createOutlookEvent(TaskDto taskDto) {
        LinkedList<Option> requestOptions = new LinkedList<>();
        requestOptions.add(new HeaderOption("Prefer", "outlook.timezone=\"Europe/Budapest\""));

        Event event = new Event();
        event.subject = taskDto.getName();
        ItemBody body = new ItemBody();
        body.contentType = BodyType.TEXT;
        body.content = taskDto.getDescription();
        event.body = body;
        DateTimeTimeZone start = new DateTimeTimeZone();
        start.dateTime = taskDto.getDate().toString();
        start.timeZone = "Europe/Budapest";
        event.start = start;
        DateTimeTimeZone end = new DateTimeTimeZone();
        end.dateTime = taskDto.getDate().toString();
        end.timeZone = "Europe/Budapest";
        event.end = end;

        event.allowNewTimeProposals = true;
        event.transactionId = UUID.randomUUID().toString();

        getGraphServiceClientDelegatedUser()
                .me()
                .calendar()
                .events()
                .buildRequest()
                .post(event);
    }

    private GraphServiceClient<?> getGraphServiceClientDelegatedUser() {
        OkHttpClient client = new OkHttpClient.Builder().addInterceptor(
                new CustomHeadersInterceptor("Authorization", "Bearer " + SecurityUtils.getCurrentUser()
                        .getAzureBearerToken())).build();
        return GraphServiceClient.builder()
                .httpClient(client)
                .buildClient();
    }
}
