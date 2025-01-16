using UnityEngine;
using UnityEngine.Networking;
using System.Collections;
using System.Collections.Generic;
using System;

public class HttpUploadPl : MonoBehaviour
{
    private string serverUrl = "http://127.0.0.1:5000/update_facts";

    void Start()
    {
        // Build the game state as JSON
        GameState gameState = GenerateGameState();
        string jsonPayload = JsonUtility.ToJson(gameState);
        Debug.Log("Generated JSON: " + jsonPayload);

        // Send the JSON to the Prolog server
        StartCoroutine(SendGameStateToServer(jsonPayload));
    }

    private GameState GenerateGameState()
    {
        return new GameState
        {
            knownActions = new List<Action>
            {
                new Action { player = "player1", piece = new Piece { left = 6, right = 6 }, side = "left" },
                new Action { player = "player2", piece = new Piece { left = 6, right = 5 }, side = "right" },
            },
            myHand = new List<Piece>
            {
                new Piece { left = 0, right = 1 },
                new Piece { left = 1, right = 4 },
                new Piece { left = 3, right = 4 },
                new Piece { left = 6, right = 6},
            }
        };
    }

    private IEnumerator SendGameStateToServer(string jsonPayload)
    {
        UnityWebRequest request = new UnityWebRequest(serverUrl, "POST");
        byte[] bodyRaw = System.Text.Encoding.UTF8.GetBytes(jsonPayload);
        request.uploadHandler = new UploadHandlerRaw(bodyRaw);
        request.downloadHandler = new DownloadHandlerBuffer();
        request.SetRequestHeader("Content-Type", "application/json");

        yield return request.SendWebRequest();

        if (request.result == UnityWebRequest.Result.ConnectionError ||
            request.result == UnityWebRequest.Result.ProtocolError)
        {
            Debug.LogError("Upload failed: " + request.error);
        }
        else
        {
            Debug.Log("Upload successful! Server responded: " + request.downloadHandler.text);
        }
    }

    [Serializable]
    public class GameState
    {
        public List<Action> knownActions;
        public List<Piece> myHand;
    }

    [Serializable]
    public class Action
    {
        public string player;
        public Piece piece;
        public string side;
    }

    [Serializable]
    public class Piece
    {
        public int left;
        public int right;
    }
}
